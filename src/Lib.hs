{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lib
( serveDNS
, Conf(..)
) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception.Safe     (bracketOnError, handle, tryAny)
import           Control.Monad
import           Control.Monad.IO.Class     (liftIO)
import           Data.Array.Unboxed
import qualified Data.ByteString            as S
import qualified Data.ByteString.Base64     as B64
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Internal   as BI
import qualified Data.ByteString.Lazy       as SL
import           Data.Char                  (toLower)
import           Data.Conduit.Attoparsec    (ParseError (..))
import           Data.IP
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text (..))
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           Data.Word
import           Database.Bloodhound        (EsPassword, EsUsername)
import           Log
import           Log.Backend.ElasticSearch
import           Log.Backend.StandardOutput
import           Network.BSD
import           Network.DNS
import           Network.Socket             hiding (recvFrom)
import           Network.Socket.ByteString
import           System.Environment
import           System.Random              (randomIO)
import           System.Timeout

import           Parse


data Conf = Conf
  { confBufSize  :: !Int
  , confTTL      :: !Int
  , confDomain   :: !Domain
  , confTimeout  :: !Int
  , confPort     :: !Int
  , confAs       :: ![IPv4]
  , confNSs      :: ![Domain]
  , confSOAemail :: !Domain
  , confHostname :: !String
  }
  deriving Show


type ESConf = (Text, Maybe (EsUsername, EsPassword))


serveDNS :: Domain -> Int -> [String] -> [String] -> String -> Maybe ESConf -> IO ()
serveDNS domain port as nss email maybeES = withSocketsDo $ do
  hostname <- getHostName
  let conf =
        Conf
        { confBufSize  = 512
        , confTTL      = 432000
        , confDomain   = domain
        , confTimeout  = 3 * 1000 * 1000
        , confPort     = port
        , confAs       = map read as
        , confNSs      = map B8.pack nss
        , confSOAemail = B8.pack email
        , confHostname = hostname
        }
  addrinfos <-
    getAddrInfo
      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
      Nothing
      (Just . show $ confPort conf)
  addrinfo <- maybe (fail "no addr info") return (listToMaybe addrinfos)
  let doit logger = do
        forkIO $ doUDP addrinfo conf logger
        doTCP addrinfo conf logger
  case maybeES of
    Nothing -> withSimpleStdOutLogger doit
    Just (url, login) -> do
      let es =
            defaultElasticSearchConfig
            { esServer  = url
            , esIndex   = "logs"
            , esMapping = "log"
            , esLogin   = login
            }
      withElasticSearchLogger es randomIO doit


doUDP :: AddrInfo -> Conf -> Logger -> IO ()
doUDP addrinfo conf logger = do
  sock <- socket (addrFamily addrinfo) Datagram defaultProtocol
  bind sock (addrAddress addrinfo)
  forever $ do
    (bs, addr) <- recvFrom sock (confBufSize conf)
    forkIO $ runLogT "UDP" logger $ handleUDP conf sock addr bs


doTCP :: AddrInfo -> Conf -> Logger -> IO ()
doTCP addrinfo conf logger = do
  sock <-
    bracketOnError
      (socket (addrFamily addrinfo) Stream defaultProtocol)
      close
      (\sock -> do
          setSocketOption sock ReuseAddr 1
          setSocketOption sock NoDelay 1
          bind sock $ addrAddress addrinfo
          listen sock $ max 1024 maxListenQueue
          return sock)
  forever $ do
    (conn, addr) <- accept sock
    forkIO $ runLogT "TCP" logger $ handleTCP conf conn addr


handleRequest :: Conf -> DNSMessage -> DNSMessage
handleRequest conf req = fromMaybe notFound go
 where
  domain = confDomain conf<>"."

  ident = identifier . header $ req

  notFound =
    DNSMessage
    { header     =
        DNSHeader
        { identifier = ident
        , flags =
            DNSFlags
            { qOrR         = QR_Response
            , opcode       = OP_STD
            , authAnswer   = False
            , trunCation   = False
            , recDesired   = True
            , recAvailable = False
            , rcode        = NoErr
            , authenData   = False
            }
        }
    , question   = question req
    , answer     = []
    , authority  = []
    , additional = []
    }

  setTTL ttl rr = rr { rrttl = ttl }

  go =
    case listToMaybe $ question req of
      Nothing -> Nothing
      Just q  ->
        let name = qname q in
        case qtype q of
          A   ->
            if (lowercase name) == (lowercase domain)
            then Just . response ident q . map (recordA name 300) $ confAs conf
            else do
              ip <- parseDomain (confDomain conf) $ name
              return . response ident q $ map (recordA name (confTTL conf)) [ip]
          NS  ->
            if (lowercase domain) `B8.isSuffixOf` (lowercase name)
            then Just . response ident q . map (recordNS name 300) $ confNSs conf
            else Nothing
          SOA ->
            Just $ response ident q [recordSOA name (head $ confNSs conf) (confSOAemail conf)]
          _  -> Nothing


handleUDP :: Conf -> Socket -> SockAddr -> S.ByteString -> LogT IO ()
handleUDP conf@Conf{..} sock addr bs =
  case decode $ SL.fromChunks [bs] of
    Right req -> do
      let rsp = handleRequest conf req
      let packet = mconcat . SL.toChunks $ encode rsp
      void $ timeout' addr confTimeout $ sendAllTo sock packet addr
      logDNS conf addr req rsp
    Left reason ->
      logAttention "Failed to decode message" $
        object
        [ "from" .= show addr
        , "reason" .= reason
        , "message" .= (decodeUtf8 $ B64.encode bs)
        , "server" .= confHostname
        ]


handleTCP :: Conf -> Socket -> SockAddr -> LogT IO ()
handleTCP conf@Conf{..} sock addr = do
  r <- tryAny $ handle handleParseError $ timeout' addr confTimeout $ receiveVC sock
  case r of
    Left err ->
      logAttention "Failed to receive request" $
        object
        [ "from" .= show addr
        , "reason" .= show err
        , "server" .= confHostname
        ]
    Right Nothing -> return ()
    Right (Just req) -> do
      let rsp = handleRequest conf req
      let bs = encode rsp
      let packet = mconcat . SL.toChunks . toLazyByteString $
                     word16BE (fromIntegral (SL.length bs)) <>
                     lazyByteString bs
      void $ timeout' addr confTimeout $ sendAll sock packet
      logDNS conf addr req rsp
  liftIO $ close sock
 where
  handleParseError :: ParseError -> LogT IO (Maybe DNSMessage)
  handleParseError err = do
    logAttention "ParseError" $
      object
      [ "from" .= show addr
      , "reason" .= errorMessage err
      , "server" .= confHostname
      ]
    return Nothing


logDNS :: Conf -> SockAddr -> DNSMessage -> DNSMessage -> LogT IO ()
logDNS conf addr req rsp = do
  case answer rsp of
    [] -> return ()
    (ResourceRecord { rdata = (RD_A ip) }):_ ->
      logInfo "" $
        object
        [ "from" .= show addr
        , "question" .= (decodeUtf8 . qname . head . question $ req)
        , "answer" .= show ip
        , "server" .= confHostname conf
        ]
    _ -> return ()


timeout' :: SockAddr -> Int -> IO a -> LogT IO (Maybe a)
timeout' addr tm io = do
  result <- liftIO $ timeout tm io
  when (isNothing result) $
    logAttention_ $ "timeout sending to "<>(T.pack $ show addr)
  return result


defaultHeader :: Int -> DNSHeader
defaultHeader ident =
  DNSHeader
  { identifier = ident
  , flags =
      DNSFlags
      { qOrR         = QR_Response
      , opcode       = OP_STD
      , authAnswer   = True
      , trunCation   = False
      , recDesired   = True
      , recAvailable = False
      , rcode        = NoErr
      , authenData   = False
      }
  }


defaultResponse :: Int -> DNSMessage
defaultResponse ident =
  DNSMessage
  { header     = defaultHeader ident
  , question   = []
  , answer     = []
  , authority  = []
  , additional = []
  }


response :: Int -> Question -> [ResourceRecord] -> DNSMessage
response ident q an =
  (defaultResponse ident)
  { question = [q]
  , answer = an
  }


recordA :: Domain -> Int -> IPv4 -> ResourceRecord
recordA dom ttl ip = ResourceRecord dom A ttl $ RD_A ip


recordNS :: Domain -> Int -> Domain -> ResourceRecord
recordNS dom ttl domain = ResourceRecord dom NS ttl $ RD_NS domain


recordSOA :: Domain -> Domain -> Domain -> ResourceRecord
recordSOA dom ns email = ResourceRecord dom SOA 432000 $ RD_SOA ns email 1 10800 3600 604800 3600

ctype_lower = listArray (0,255) (map (BI.c2w . toLower) ['\0'..'\255']) :: UArray Word8 Word8

lowercase :: S.ByteString -> S.ByteString
lowercase = S.map (\x -> ctype_lower!x)
