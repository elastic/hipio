{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lib
( serveDNS
, Conf(..)
) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString            as S
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as SL
import           Data.IP
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text (..))
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           Database.Bloodhound
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
  }
  deriving Show


type ESConf = (Text, Maybe (EsUsername, EsPassword))


serveDNS :: Domain -> Int -> [String] -> [String] -> String -> Maybe ESConf -> IO ()
serveDNS domain port as nss email maybeES = withSocketsDo $ do
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
        }
  addrinfos <-
    getAddrInfo
      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
      Nothing
      (Just . show $ confPort conf)
  addrinfo <- maybe (fail "no addr info") return (listToMaybe addrinfos)
  sockUDP <- socket (addrFamily addrinfo) Datagram defaultProtocol
  bind sockUDP (addrAddress addrinfo)
  sockTCP <- socket AF_INET Stream defaultProtocol
  setSocketOption sockTCP ReuseAddr 1
  setSocketOption sockTCP NoDelay 1
  bind sockTCP (SockAddrInet (fromIntegral $ confPort conf) iNADDR_ANY)
  listen sockTCP (max 1024 maxListenQueue)
  let doit logger = do
        forkIO . forever $ do
          (bs, addrUDP) <- recvFrom sockUDP (confBufSize conf)
          forkIO $ runLogT "" logger $ handlePacketUDP conf sockUDP addrUDP bs
        forever $ do
          (sock, addrTCP) <- accept sockTCP
          forkIO $ runLogT "" logger $ handlePacketTCP conf sock addrTCP

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
        , flags = DNSFlags {
              qOrR         = QR_Response
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
            if name == domain
            then Just . response ident q . map (recordA name 300) $ confAs conf
            else do
              ip <- parseDomain (confDomain conf) $ name
              return . response ident q $ map (recordA name (confTTL conf)) [ip]
          NS  ->
            if domain `B8.isSuffixOf` name
            then Just . response ident q . map (recordNS name 300) $ confNSs conf
            else Nothing
          SOA ->
            Just $ response ident q [recordSOA name (head $ confNSs conf) (confSOAemail conf)]
          _  -> Nothing


handlePacketUDP :: Conf -> Socket -> SockAddr -> S.ByteString -> LogT IO ()
handlePacketUDP conf@Conf{..} sock addr bs =
  let sender packet = sendAllTo sock packet addr
  in handlePacket conf addr bs sender


handlePacketTCP :: Conf -> Socket -> SockAddr -> LogT IO ()
handlePacketTCP conf sock addr = do
  bs <- liftIO $ Network.Socket.ByteString.recv sock 4096
  let sender = sendAll sock
  handlePacket conf addr bs sender


handlePacket :: Conf -> SockAddr -> S.ByteString -> (S.ByteString -> IO ()) -> LogT IO ()
handlePacket conf@Conf{..} addr bs sender =
  case decode $ SL.fromChunks [bs] of
    Right req -> do
      let rsp = handleRequest conf req
      let packet = mconcat . SL.toChunks $ encode rsp
      void $ timeout' addr confTimeout (sender packet)
      case answer rsp of
        [] -> return ()
        (ResourceRecord { rdata = (RD_A ip) }):_ ->
          logInfo "" $ object [
              "from" .= (show addr)
            , "question"  .= (decodeUtf8 . qname . head . question $ req)
            , "answer" .= show ip
            ]
        _ -> return ()
    Left msg ->
      logAttention "Failed to decode message" $ object [ "message" .= msg ]


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
  , flags = DNSFlags {
        qOrR         = QR_Response
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
