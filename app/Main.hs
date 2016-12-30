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
  opts <- execParser opts
  esconf <-
    case optsEsUrl opts of
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
  let ns = case optsNSs opts of
             [] -> [optsDomain opts]
             ns -> ns
  serveDNS
    (B8.pack $ optsDomain opts)
    (optsPort opts)
    (optsAs opts)
    ns
    (optsEmail opts)
    esconf
 where
   opts = info (helper <*> options)
     ( fullDesc
    <> header "hipio - Wildcard DNS Server for any IP Address"
    <> progDesc
        "hipio maps <anything>.<IP Address>.<domain> to the corresponding <IP Address>,\n \
       \ e.g. 127.0.0.1.<domain> maps to 127.0.0.1"
     )


data Options = Options
  { optsDomain :: String
  , optsPort   :: Int
  , optsEsUrl  :: Maybe Text
  , optsAs     :: [String]
  , optsNSs    :: [String]
  , optsEmail  :: String
  }


options :: Parser Options
options =
  Options
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
    <> help "Listening port."
    )
  <*>
    optional (
      textOption
      (  long "es"
      <> help "Elasticsearch URL for Logging. Set `ES_USER` and `ES_PASS` environment variables for Basic Auth."
      <> metavar "URL"
      ))
  <*>
    many (
      strOption
      (  short 'a'
      <> help "A record for DOMAIN"
      <> metavar "RECORD"
      ))
  <*>
    many (
      strOption
      (  long "ns"
      <> help "NS record for DOMAIN"
      <> metavar "RECORD"
      ))
  <*>
    strOption
    (  long "soa-email"
    <> help "Email address for SOA record. Example: admin.example.com"
    <> metavar "EMAIL"
    )
