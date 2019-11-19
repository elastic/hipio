{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.ByteString.Char8    as B8
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Lib
import           Options.Applicative
import           Options.Applicative.Text (textOption)
import           System.Environment

main :: IO ()
main = do
  opts <- execParser opts
  let ns = case optsNSs opts of
             [] -> [optsDomain opts]
             ns -> ns
  serveDNS
    (B8.pack $ optsDomain opts)
    (optsPort opts)
    (optsAs opts)
    ns
    (optsEmail opts)
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
