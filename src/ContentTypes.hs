{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- | Type class instance to handle plain ASCII by decoding using UTF8 decoder.
--
--   This is useful for handling content type "text/plain", without declared encoding.
--
--   TODO: for extra validation, we might want to check if it is 7-bit clean first.
module ContentTypes(AsciiText,HTML(..)) where

-- Library imports
import           Control.Arrow                     ( left )
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List.NonEmpty                (NonEmpty((:|)))
import           Data.Text                         (Text,pack)
import qualified Data.Text.Encoding         as Enc
import           Network.HTTP.Media.MediaType      ((//), (/:))
import           Servant.API.ContentTypes          ( Accept(..), MimeUnrender(..), MimeRender(..) )
import           Data.String(IsString(..))


-- | Empty type for declaring 7-bit ASCII content encoding.
--
--   Note that 7-bit clean text is preserved by UTF8 encoding.
data AsciiText
instance Accept AsciiText where
  contentType _ = "text" // "plain"

instance MimeUnrender AsciiText Text where
    -- UTF8 encoding is backwards compatible with 7-bit clean ASCII.
    mimeUnrender _ = left show . Enc.decodeUtf8' . BSL.toStrict

instance MimeRender AsciiText Text where
    mimeRender _ = BSL.fromStrict . Enc.encodeUtf8

-- | HTML content type
--
--   Use for short HTML fragments only, otherwise switch to Lucid.
newtype HTML = HTML { unHTML :: Text }

instance Accept HTML where
   contentTypes _ = ("text" // "html" /: ("charset", "utf-8"))
                 :| ["text" // "html"] -- since we expect to use only a 7-bit clean HTML fragment

instance MimeRender HTML HTML where
  mimeRender _ = BSL.fromStrict . Enc.encodeUtf8 . unHTML

instance MimeRender HTML Text where
  mimeRender _ = BSL.fromStrict . Enc.encodeUtf8

instance IsString HTML where
  fromString = HTML . pack
