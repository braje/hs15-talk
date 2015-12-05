{-# LANGUAGE OverloadedStrings #-}

module CommonReflex where

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Types (castToHTMLDocument)--hiding (Widget, unWidget, Event)

--import Reflex.Class
import Reflex.Host.Class
import Reflex.Spider (Spider, SpiderHost (..))
--import Reflex.Dom.Class
import Reflex.Dom.Internal

import           Control.Monad


runReflexWidget :: String ->
                   Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () ->
                   IO ()
runReflexWidget elemId w = runWebGUI $ \webView -> do
  Just doc <- liftM (fmap castToHTMLDocument) $ webViewGetDomDocument webView
  Just elem <- liftM (fmap castToHTMLElement) $ getElementById doc elemId
  attachWidget elem webView w
