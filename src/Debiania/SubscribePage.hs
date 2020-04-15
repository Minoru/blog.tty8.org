{-# LANGUAGE OverloadedStrings #-}

module Debiania.SubscribePage (
    subscribePageRules
) where

import Hakyll

import Debiania.Compilers
import Debiania.Context

subscribePageRules :: Rules ()
subscribePageRules = do
    let ctx = rootUrlCtx <> defaultContext

    create ["subscribe.markdown"] $ do
        route   $ setExtension "html"
        compile $ debianiaCompiler
          >>= loadAndApplyTemplate "templates/about.html" ctx
          >>= loadAndApplyTemplate
                "templates/default.html"
                (constField "navbar-subscribe" "Yep" <> ctx)
          >>= relativizeUrls

    create ["subscribe.markdown"] $ version "gzipped" $ do
        route   $ setExtension "html.gz"
        compile gzipFileCompiler
