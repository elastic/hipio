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
  { confBufSize :: Int
  , confTTL     :: Int
  , confDomain  :: Domain
  , confTimeout :: Int
  , confPort    :: Int
  }
  deriving Show


type ESConf = (Text, Maybe (EsUsername, EsPassword))


serveDNS :: Domain -> Int -> Maybe ESConf -> IO ()
serveDNS domain port maybeES = withSocketsDo $ do
  let conf =
        Conf
        { confBufSize = 512
        , confTTL     = 432000
        , confDomain  = domain
        , confTimeout = 3 * 1000 * 1000
        , confPort    = port
        }
  addrinfos <-
    getAddrInfo
      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
      Nothing
      (Just . show $ confPort conf)
  addrinfo <- maybe (fail "no addr info") return (listToMaybe addrinfos)
  sock <- socket (addrFamily addrinfo) Datagram defaultProtocol
  bind sock (addrAddress addrinfo)
  let doit logger =
        forever $ do
          (bs, addr) <- recvFrom sock (confBufSize conf)
          forkIO $ runLogT "" logger $ handlePacket conf sock addr bs
  case maybeES of
    Nothing -> withSimpleStdOutLogger doit
    Just (url, login) -> do
      let es =
            ElasticSearchConfig
            { esServer = url
            , esIndex = "logs"
            , esMapping = "log"
            , esLogin = login
            }
      withElasticSearchLogger es randomIO doit


handleRequest :: Conf -> DNSMessage -> DNSMessage
handleRequest conf req = fromMaybe notFound parseHosts
 where
  filterA = filter ((==A) . qtype)
  ident = identifier . header $ req
  notFound =
    defaultResponse
    { header = (header defaultResponse) { identifier = ident }
    , question = question req
    }
  parseHosts = do
    q <- listToMaybe . filterA . question $ req
    let ip = maybeToList $ parseDomain (confDomain conf) $ qname q
        rsp = responseA ident q ip
        setTTL rr = rr { rrttl = confTTL conf }
        hd = header rsp
        flgs = (flags hd)
               { recAvailable = False
               , authAnswer = not $ Prelude.null ip
               }
    return $
      rsp
      { header = hd { flags = flgs }
      , answer = map setTTL $ answer rsp
      }


handlePacket :: Conf -> Socket -> SockAddr -> S.ByteString -> LogT IO ()
handlePacket conf@Conf{..} sock addr bs =
  case decode $ SL.fromChunks [bs] of
    Right req -> do
      let rsp = handleRequest conf req
      let packet = mconcat . SL.toChunks $ encode rsp
      void $ timeout' addr confTimeout (sendAllTo sock packet addr)
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




defaultQuery :: DNSMessage
defaultQuery = DNSMessage {
    header = DNSHeader {
       identifier = 0
     , flags = DNSFlags {
           qOrR         = QR_Query
         , opcode       = OP_STD
         , authAnswer   = False
         , trunCation   = False
         , recDesired   = True
         , recAvailable = False
         , rcode        = NoErr
         , authenData   = False
         }
     }
  , question   = []
  , answer     = []
  , authority  = []
  , additional = []
  }


defaultResponse :: DNSMessage
defaultResponse =
  let hd = header defaultQuery
      flg = flags hd
  in  defaultQuery {
        header = hd {
          flags = flg {
              qOrR = QR_Response
            , authAnswer = False
            , recAvailable = False
            , authenData = False
            }
        }
      }
