-- | This module defines shortened URLs as numbers encoded on a base of characters allowed in URLs.
--
--   This module exposes internals for testing.
module URLId.Internal where

import qualified Data.Vector as V
import qualified Data.Text   as T
import           Data.Char        ( ord )
import           Data.List        ( unfoldr )

-- * Encoding of URLs as numbers in a based dictated by a selected range of characters
-- | Datatype for numbers encoded in base-62
newtype URLId = URLId Integer
  deriving (Eq)

instance Bounded URLId where
  minBound = URLId  0
  maxBound = URLId (maxRange-1)

-- | Base for URL positional index
urlBase :: Integer
urlBase  = fromIntegral $ V.length alphabet

-- | Alphabet
alphabet :: V.Vector Char
alphabet = V.fromList $ latinDigits <> latinLower <> latinUpper

-- | Character classes serving as digits.
--
--   These are lists of characters to be used, not Strings.
{-# HLINT ignore "Use String" #-}
latinDigits, latinLower, latinUpper :: [Char]
latinDigits = ['0'..'9']
latinLower  = ['a'..'z']
latinUpper  = ['A'..'Z']

numLatinDigits :: Integer
numLatinDigits = fromIntegral $ length latinDigits

numLatinLowers :: Integer
numLatinLowers = fromIntegral $ length latinLower

-- | Convert a single Char to a numerical value of a single digit from the @alphabet.
urlBaseDigit :: Char -> Maybe Integer
urlBaseDigit c | ord c >= ord '0' && ord c <= ord '9' = Just $                  fromIntegral (ord c - ord '0')
urlBaseDigit c | ord c >= ord 'a' && ord c <= ord 'z' = Just $ numLatinDigits + fromIntegral (ord c - ord 'a')
urlBaseDigit c | ord c >= ord 'A' && ord c <= ord 'Z' = Just $ numLatinDigits
                                                             + numLatinLowers + fromIntegral (ord c - ord 'A')
urlBaseDigit _                                        = Nothing

-- | Convert a single digit value into an alphabetic digit.
toAlphabeticDigit :: Integer -> Char
toAlphabeticDigit i | 0<=i && i < urlBase = alphabet V.! fromIntegral i
toAlphabeticDigit i                       = error $ "Digit index out of range: " <> show i

-- | Length of shortened URL.
urlLength :: Integer
urlLength  = 6

-- | Given a base, convert a list of digit positions into a number.
--   Most significant digit comes first.
digitsToNumber :: Integer -> [Integer] -> Integer
digitsToNumber aBase = foldl next 0
  where
    next aSum nextDigit = aSum*aBase+nextDigit

-- | Given a base, and a number, yield a list of digit values for positions.
--   Most significant digit comes first.
numberToDigits :: Integer -> Integer -> [Integer]
numberToDigits base = reverse . unfoldr next
  where
    next 0 = Nothing
    next n = Just (r,q)
      where
        (q, r) = n `quotRem` base

instance Show URLId where
  showsPrec _ i = ('"':) . (urlidString i++) . ('"':)

-- | Convert URLId number to string
urlidString :: URLId -> String
urlidString (URLId i) = toAlphabeticDigit <$> numberToDigits urlBase i

-- | Convert URLId number to text
urlidText :: URLId -> T.Text
urlidText = T.pack . urlidString

-- | Read text as URLId number.
textToURLId  :: T.Text -> Maybe URLId
textToURLId t = do
  digits <- mapM urlBaseDigit $ T.unpack t
  mkURLId $ fromIntegral $ digitsToNumber urlBase digits

-- | Maximum index of URL.
maxRange :: Integer
maxRange = urlBase^urlLength

-- | Constructor checking that we never cross the limits of the number.
mkURLId :: Integer -> Maybe URLId
mkURLId i | i < maxRange && i >= 0 = Just $ URLId i
mkURLId _                          = Nothing -- Trying to generate a number outside allowed URL range.

-- | Convert URL to Integer.
urlIdToInteger :: URLId -> Integer
urlIdToInteger (URLId i) = i