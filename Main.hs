{-# LANGUAGE OverloadedStrings #-}
import Web.Wheb
import Web.Wheb.Plugins.Redis

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (stringValue)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import qualified Data.ByteString.Char8 as BC
import qualified System.Random as SR
import           Control.Monad (replicateM)
import           Text.Read (readMaybe)
import qualified Data.Text as T
import Data.Maybe

import Network.URI (parseURI, uriPath)

data WhebShort = WhebShort RedisContainer
type ShortHandler = WhebHandler WhebShort ()

instance RedisApp WhebShort where
  getRedisContainer (WhebShort rc) = rc

renderHtml :: H.Html -> ShortHandler
renderHtml = builder "text/html" . renderHtmlBuilder

mainForm' :: H.Html
mainForm' = H.form H.! A.action "." H.! A.method "post" $ do
  H.label H.! A.for "url" $ "URL"
  H.input H.! A.type_ "text" H.! A.name "url" H.! A.id "url"
  H.button "Shorten"

layout :: H.Html -> H.Html
layout html = H.docTypeHtml $ do
    H.head $ do
      H.title "Wheb Shortener"
    H.body $ html

handleHome :: ShortHandler
handleHome = do
  renderHtml $ layout $ do
      H.h1 "Shortener in Wheb Example"
      mainForm'


newShortString = do
  let gen = replicateM 7 (randomElement alphaNum)
  liftIO gen

  where
    alphaNum = ['A'..'Z'] ++ ['0'..'9']
    randomElement l = SR.randomRIO (0, ((length l) - 1)) >>= \d -> return (l !! d)

shortenUrl :: ShortHandler
shortenUrl = do
  urlM <- getPOSTParam "url"
  short <- newShortString
  case urlM of
    Just url -> do
      let originalUrl = T.unpack url
      let uriM = parseURI originalUrl
      case uriM of
        Just uri -> do
          runRedis $ set (BC.pack short) (BC.pack originalUrl)
          renderHtml $ layout $ do
            H.h1 "Wheb Shortener"
            let fullShort = "http://localhost:3000/s/" ++ short
            H.p $ "Original URL: "
            H.p $ (H.toHtml originalUrl)
            H.p $ "Shortened:"
            H.p $
              H.a H.! A.href (stringValue fullShort) $ (H.toHtml fullShort)
            mainForm'
        Nothing -> do
          renderHtml $ layout $ do
            H.p "Enter a valid URL."
            mainForm'
    Nothing -> do
      renderHtml $ layout $ do
        H.p "Enter a valid URL."
        mainForm'

expandUrl :: ShortHandler
expandUrl = do
  code <- getRouteParam "code"
  redisE <- runRedis $ get (BC.pack (T.unpack code))
  case redisE of
    Left reply ->
      renderHtml $ layout $ do
        H.p "Not found"
    Right urlM ->
      case urlM of
        Just url ->
          redirect (T.pack (BC.unpack url))
        Nothing ->
          renderHtml $ layout $ do
            H.p "Not found"


main :: IO ()
main = do
  opts <- generateOptions $ do
    r <- initRedis defaultConnectInfo
    addGET "home" rootPat handleHome
    addPOST "shorten" rootPat shortenUrl
    addGET "expand" (rootPat </> "s" </> (grabText "code")) expandUrl
    return (WhebShort r, ())

  runWhebServer opts
