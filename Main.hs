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

renderHtml :: H.Html -> MinHandler
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

handleHome :: MinHandler
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

shortenUrl :: MinHandler
shortenUrl = do
  urlM <- getPOSTParam "url"
  short <- newShortString
  renderHtml $ layout $ do
    H.h1 "Wheb Shortener"
    case urlM of
      Just url -> do
        let fullShort = "http://localhost:3000/" ++ short
        let originalUrl = T.unpack url
        H.p $ "Original URL: "
        H.p $ (H.toHtml originalUrl)
        H.p $ "Shortened:"
        H.p $
          H.a H.! A.href (stringValue fullShort) $ (H.toHtml fullShort)
      Nothing -> do
        H.p "Enter a valid URL."
    mainForm'


main :: IO ()
main = do
  opts <- genMinOpts $ do
    r <- initRedis defaultConnectInfo
    addGET "." rootPat handleHome
    addPOST "url" rootPat shortenUrl
  runWhebServer opts
