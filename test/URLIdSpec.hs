{-# OPTIONS_GHC -fno-warn-orphans #-}
module URLIdSpec(spec) where

import           Data.Maybe (fromJust)
import qualified Data.Vector as V

import           URLId.Internal as URLId

import           Test.QuickCheck          
import           Test.Hspec ( describe, it, shouldBe, Spec )
import           Test.Hspec.QuickCheck ( prop )

instance Arbitrary URLId where
  arbitrary = fromJust . mkURLId <$> choose (0, URLId.maxRange-1)

spec :: Spec
spec = describe "URLId" $ do 
  it "test that alphabet covers consecutive digits" $
    (urlBaseDigit <$> alphabet) `shouldBe` Just <$>  V.fromList [0..fromIntegral (length alphabet)-1]
  prop "test that urlBaseDigit is inverse of toAlphabeticDigit" $ do
    aDigitValue <- fromIntegral <$> choose (0, length alphabet-1)
    return       $ urlBaseDigit (toAlphabeticDigit aDigitValue) `shouldBe` Just aDigitValue
  prop "digitsToNumber is inverse of numberToDigits" $ do
    Positive urlid   <- arbitrary
    return $ digitsToNumber urlBase (numberToDigits urlBase urlid) `shouldBe` urlid
  prop "textToURLId is inverse of urlidText" $ do
    urlid   <- arbitrary
    return $ textToURLId (urlidText urlid) `shouldBe` Just urlid
