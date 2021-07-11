{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import qualified Data.Text.Encoding as T(encodeUtf8)
import qualified Data.Text          as T
import           Data.ByteString        (ByteString)
import API (app)
import Test.Hspec ( hspec, describe, it, Spec )
import Test.Hspec.Wai
import Network.Wai.Test(simpleBody)
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types.Method ( methodPost )

import qualified URLIdSpec (spec)

main :: IO ()
main = hspec $ do
         URLIdSpec.spec
         serverSpec

testURL :: T.Text
testURL = "https://google.com"

testURLBS :: ByteString
testURLBS  = T.encodeUtf8 testURL

serverSpec :: Spec
serverSpec = with app $
  describe "server" $ do
    it "too long after shortening gives 400" $
        get "/abrakadabradwakije" `shouldRespondWith` 400
    it "unregistered path responds with 404" $
        get "/azizel" `shouldRespondWith` 404
    
    describe "/shorten" $ do
        it "responds to" $
            request methodPost "/shorten"
                    [("Content-Type", "text/plain;charset=utf-8")] (BSL.fromStrict testURLBS)
              `shouldRespondWith` 200
        it "responds with no encoding declared" $
            request methodPost "/shorten"
                    [("Content-Type", "text/plain")] (BSL.fromStrict testURLBS)
              `shouldRespondWith` 200
        it "responds to form input" $
            postHtmlForm "/shorten" 
                         [("url", T.unpack testURL)]
              `shouldRespondWith` 200

    -- TODO: Here we should really test it with a range of test URLs.
    --       Since Test.Hspec.Wai is not yet finished, I would postpone it until
    --       having sufficient time to write a decent Wai random testing library.
    --
    --       Note that `servant-quickcheck` and `roboservant` do not fit the bill here,
    --       since they check general behaviour of the API, not the intended functionality.
    it "redirects after /shorten" $ do
        response <- request methodPost "/shorten" [("Content-Type", "text/plain;charset=utf-8")]
                            (BSL.fromStrict testURLBS)
        liftIO $ print response
        let path = BSL.toStrict $ simpleBody response
        get ("/" <> path) `shouldRespondWith` 302 { matchHeaders = ["Location" <:> testURLBS] } -- redirect

