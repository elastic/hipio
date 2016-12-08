module Parse where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (void)
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8
import           Data.IP
import           Network.DNS

parseDomain :: Domain -> Domain -> Maybe IPv4
parseDomain rootDomain domain =
  case parseOnly (parser rootDomain) domain of
    Left _ -> Nothing
    Right x -> Just x

dot :: Parser ()
dot = void $ char '.'

parser :: Domain -> Parser IPv4
parser root = parseDomain
 where
  ipAndRoot = do
    r <- ip
    void $ dot *> string root <* dot <* endOfInput
    return r
  parseDomain = try ipAndRoot <|> (anyChar `manyTill` dot *> parseDomain)

ip :: Parser IPv4
ip = do
  a <- octet
  void $ dot
  b <- octet
  void $ dot
  c <- octet
  void $ dot
  d <- octet
  return $ toIPv4 [a,b,c,d]
 where
  octet = decimal >>= limitSize
  limitSize i
    | i > 255   = fail "All octets in an ip address must be between 0 and 255"
    | otherwise = return i
