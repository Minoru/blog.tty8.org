{-# LANGUAGE OverloadedStrings #-}

module Debiania.SubscribePage (
    subscribePageRules
) where

import Data.Monoid ((<>))

import Hakyll

import Debiania.Compilers
import Debiania.Settings

subscribePageRules :: Rules ()
subscribePageRules = do
    let ctx = rootUrlCtx <> defaultContext

    create ["subscribe.markdown"] $ do
        route   $ setExtension "html"
        compile $ debianiaCompiler
          >>= loadAndApplyTemplate "templates/about.html" ctx
          >>= loadAndApplyTemplate
                "templates/default.html"
                (ctx <> constField "navbar-subscribe" "Yep")
          >>= relativizeUrls

    create ["subscribe.markdown"] $ version "gzipped" $ do
        route   $ setExtension "html.gz"
        compile gzipFileCompiler
