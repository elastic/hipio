{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lib
( serveDNS
, Conf(..)
) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString           as S
import           Data.ByteString.Lazy      hiding (filter, length, map,
                                            putStrLn)
import           Data.IP
import           Data.Maybe
import           Data.Monoid
import           Network.BSD
import           Network.DNS
import           Network.Socket            hiding (recvFrom)
import           Network.Socket.ByteString
import           System.Environment
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

timeout' :: String -> Int -> IO a -> IO (Maybe a)
timeout' msg tm io = do
  result <- timeout tm io
  maybe (putStrLn msg) (const $ return ()) result
  return result


handleRequest :: Conf -> DNSMessage -> Maybe DNSMessage
handleRequest conf req = parseHosts
 where
  filterA = filter ((==A) . qtype)
  ident = identifier . header $ req
  mq = listToMaybe . filterA . question $ req
  parseHosts =
    case mq of
      Nothing -> Nothing
      Just q ->
        let an = maybeToList $ parseDomain (confDomain conf) $ qname q
            rsp = responseA ident q an
            setTTL rr = rr { rrttl = confTTL conf }
          in Just $
             rsp
             { header =
                let hd   = header rsp
                    flgs = (flags hd)
                           { recAvailable = False
                           , authAnswer = not $ Prelude.null an
                           }
                 in hd { flags = flgs }
             , answer = map setTTL $ answer rsp
             }


handlePacket :: Conf -> Socket -> SockAddr -> S.ByteString -> IO ()
handlePacket conf@Conf{..} sock addr bs =
  case decode $ fromChunks [bs] of
    Right req -> do
      case handleRequest conf req of
        Just rsp ->
          let packet = mconcat . toChunks $ encode rsp
          in void $ timeout' "send timeout" confTimeout (sendAllTo sock packet addr)
        Nothing -> return ()
    Left msg -> putStrLn msg


serveDNS :: Domain -> Int -> IO ()
serveDNS domain port = withSocketsDo $ do
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
  forever $ do
    (bs, addr) <- recvFrom sock (confBufSize conf)
    forkIO $ handlePacket conf sock addr bs
