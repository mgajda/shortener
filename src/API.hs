{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
-- | API server with Servant types.
module API
    ( startApp
    , app
    ) where

-- Library imports
import           Control.Monad.IO.Class            ( liftIO )
import           Data.Text                         (Text)
import           Network.HTTP.Types.Status         (status415)
import           Network.Wai                       ( responseLBS )
import           Network.Wai.Handler.Warp          ( run )
import           Servant
import           System.IO                         (hPutStrLn, stderr)
import           Web.FormUrlEncoded

-- Inside this package:
import           DB ( withDB, DB, lookupURL, newURL )
import           URLId
import           ContentTypes(AsciiText, HTML(..))

-- * URL to shorten
-- | URL input alias for handling different content types
newtype URLToShorten = URLToShorten { _url :: Text }

instance MimeUnrender PlainText URLToShorten where
    mimeUnrender proxy bs = URLToShorten <$> mimeUnrender proxy bs

instance MimeUnrender AsciiText URLToShorten where
    mimeUnrender proxy bs = URLToShorten <$> mimeUnrender proxy bs

instance FromForm URLToShorten where
  fromForm f = URLToShorten <$> parseUnique "url" f

-- | Shortened URL path within server
type ShortenedURL = Text

-- | Shortening URL.
--   Declaring multiple content types for easy use from either HTML form
--   and text in two most popular encodings (UTF-8 and 7-bit clean ASCII).
type ShortenEndpoint =
    "shorten" :> ReqBody '[PlainText, AsciiText,FormUrlEncoded] URLToShorten
              :> Post    '[PlainText, AsciiText]                ShortenedURL

-- | Shorten an URL
handleNewURL :: DB -> Server ShortenEndpoint
handleNewURL db (URLToShorten url) = do
  result <- liftIO $ newURL db url
  case result of
    Nothing        -> throwError err503 -- temporarily unavailable
    Just shortened -> return $ urlidText shortened

-- * Using redirection
-- | Redirect
--   This endpoint captures part of URL and just returns 302 redirect header.
type RedirectEndpoint = Capture "shortened" ShortenedURL
                     :> Verb 'GET 302 '[HTML] (Headers '[Header "Location" Text] HTML)

-- | Handling shortened URL
handleShortenedURL :: DB -> Server RedirectEndpoint
handleShortenedURL db shortened =
  case textToURLId shortened of
    Nothing -> throwError err400
    Just t  -> do
      maybeUrl <- liftIO $ lookupURL db t
      case maybeUrl of
        Nothing  -> throwError err404
        Just url -> redirects url

-- * Handling redirection
-- | Redirection handler
redirects    :: Text -- ^ Target URL
             -> Handler (Headers '[Header "Location" Text] HTML)
redirects url =
    return $
      addHeader          url $
        redirectTemplate url

-- | Short HTML document for failover redirection.
redirectTemplate :: Text -> HTML
redirectTemplate url = HTML $ mconcat [
     "<html><head>"
    ,"  <meta http-equiv='Refresh' content='0; URL=" <> url <> "'>"
    ,"</head></html>"
    ]

-- * Test form.
type FormEndpoint = Get '[HTML] HTML

-- | Just returns the template.
--
--   For serious use static WAI serving.
handleForm :: Handler HTML
handleForm = return form

form :: HTML
form = HTML $ mconcat [
    "<html><body>"
  , "<form action='/shorten' method='post'>"
  , "<label for='url'>Enter URL:</label><br>"
  , "<input type='text' id='url' name='url' placeholder='http://example.com/user'><br>"
  , "</body></html>"
  ]

-- * Assemble server
-- | Whole API declaration
type API =  FormEndpoint
       :<|> ShortenEndpoint
       :<|> RedirectEndpoint
       :<|> Raw

startApp :: IO ()
startApp = run 8080 =<< app

app :: IO Application
app  = withDB $ \db -> return $ serve (Proxy :: Proxy API) $ server db

-- | Serve two endpoints, and handler for debugging malformed requests.
server :: DB -> Server API
server db =  handleForm
        :<|> handleNewURL       db
        :<|> handleShortenedURL db
        :<|> handleMalformedRequest

-- | Debugging malformed requests by logging them.
--
--   For prod, we might want to gate this with a limit.
handleMalformedRequest :: Server Raw
handleMalformedRequest = Tagged $ \req respondWith -> do
  -- NOTE: This should be redirected to the log library, if such one is used.
  hPutStrLn stderr $ "Malformed request:" <> show req
  respondWith $ responseLBS status415 [] ""
