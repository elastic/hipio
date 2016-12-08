module Main where

import Lib
import Options.Applicative
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = do
  (domain, port) <- execParser opts
  serveDNS (B8.pack domain) port
 where
   opts = info (helper <*> options)
     ( fullDesc
    <> header "hipio - Wildcard DNS Server for any IP Address"
    <> progDesc
        "hipio maps <anything>.<IP Address>.<domain> to the corresponding <IP Address>,\n \
       \ e.g. 127.0.0.1.<domain> maps to 127.0.0.1"
     )

options :: Parser (String, Int)
options =
  (,)
  <$> (argument str (metavar "DOMAIN"))
  <*> (argument auto
       ( metavar "PORT"
       <> value 53
       ))
