{-# LANGUAGE OverloadedStrings #-}
module ParseSpec where

import           Control.Monad           (forM_)
-- import           Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8   as B8
import           Data.IP
import           Data.Monoid             ((<>))
import           Network.DNS
import           Test.Hspec
import           Test.Hspec.Attoparsec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Unicode

import           Parse

valid :: [(Domain, Domain, IPv4)]
valid =
  [ ("hip.io", "0.0.0.0", "0.0.0.0")
  , ("hip.io", "127.0.0.1", "127.0.0.1")
  , ("hip.io", "a.127.0.0.1", "127.0.0.1")
  , ("hip.io", "0.a.127.0.0.1", "127.0.0.1")
  , ("hip.io", "127.0.0.a.127.0.0.1", "127.0.0.1")
  , ("foo.com", "0.1.2.3.4", "1.2.3.4")
  , ("foo.com", "a.255.1.2.3.4", "1.2.3.4")
  , ("a", "255.1.2.3.4", "1.2.3.4")
  , ("a", "256.1.2.3.4", "1.2.3.4")
  , ("hip.io", "256.1.2.3.4.5", "2.3.4.5")
  ]


spec :: Spec
spec = do
  describe "parseDomain" $ do
    forM_ valid $ \(root, d, ip) -> do
      let domain = d<>"."<>root<>"."
      it ("parses "++show domain) $
        domain ~> parser root `shouldParse` ip
    modifyMaxSuccess (const 100000) $ do
      prop "parses any IP" prop_ip
      prop "parses any domain and IP" prop_domain_ip
      prop "parses any domain and IP with any prefix" prop_prefix


instance Arbitrary IPv4 where
  arbitrary = toIPv4 <$> sequence [ arbitrary | _ <- [1..4] ]

instance Arbitrary B8.ByteString where
  arbitrary = fmap B8.pack string

prop_ip :: IPv4 -> Bool
prop_ip = prop_domain_ip "es.io"

prop_domain_ip :: Domain -> IPv4 -> Bool
prop_domain_ip rootDomain ip = parseDomain rootDomain (bshow ip<>"."<>rootDomain<>".") == Just ip

prop_prefix :: String -> SafeString -> IPv4 -> SafeString -> Bool
prop_prefix _         _                   _  (SafeString "")         = True -- ignore empty root domains
prop_prefix preprefix (SafeString prefix) ip (SafeString rootDomain) =
  parseDomain (B8.pack rootDomain) q == Just ip
 where
  q = B8.pack $ prefixStr<>ipStr<>"."<>rootDomain<>"."
  ipStr = show ip
  prefixStr =
    case prefix of
      "" -> ""
      p  -> preprefix<>p<>"."


bshow :: Show a => a -> B8.ByteString
bshow = B8.pack . show

genSafeChar :: Gen Char
genSafeChar = elements $ '.':'-':['a'..'z']++['A'..'Z']

genSafeString :: Gen String
genSafeString = listOf genSafeChar

newtype SafeString = SafeString { unwrapSafeString :: String }
    deriving Show

instance Arbitrary SafeString where
    arbitrary = SafeString <$> genSafeString
