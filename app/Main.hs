{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.ByteString.Char8 as B8
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Database.Bloodhound   (EsPassword (..), EsUsername (..))
import           Lib
import           Options.Applicative
import           Options.Applicative.Text (textOption)
import           System.Environment

main :: IO ()
main = do
  (domain, port, mbESUrl) <- execParser opts
  esconf <-
    case mbESUrl of
      Nothing -> return Nothing
      Just url -> do
        mbUN <- lookupEnv "ES_USER"
        login <-
          case mbUN of
            Nothing -> return Nothing
            Just un -> do
              !pw <- getEnv "ES_PASS"
              return $ Just (EsUsername $ T.pack un, EsPassword $ T.pack pw)
        return $ Just (url, login)
  serveDNS (B8.pack domain) port esconf
 where
   opts = info (helper <*> options)
     ( fullDesc
    <> header "hipio - Wildcard DNS Server for any IP Address"
    <> progDesc
        "hipio maps <anything>.<IP Address>.<domain> to the corresponding <IP Address>,\n \
       \ e.g. 127.0.0.1.<domain> maps to 127.0.0.1"
     )

options :: Parser (String, Int, Maybe Text)
options =
  (,,)
  <$>
    argument str
    (  metavar "DOMAIN"
    <> help "Root wildcard domain."
    )
  <*>
    argument auto
    ( metavar "PORT"
    <> value 53
    <> showDefault
    <> help "Listening UDP port."
    )
  <*>
    optional (
      textOption
      (  long "es"
      <> help "Elasticsearch URL for Logging. Set `ES_USER` and `ES_PASS` environment variables for Basic Auth."
      <> metavar "URL"
      ))
