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
    create ["subscribe.markdown"] $ do
        route   $ setExtension "html"
        compile $ debianiaCompiler
          >>= loadAndApplyTemplate "templates/about.html" debianiaCtx
          >>= loadAndApplyTemplate
                "templates/default.html"
                (debianiaCtx <> constField "navbar-subscribe" "Yep")
          >>= relativizeUrls

    create ["subscribe.markdown"] $ version "gzipped" $ do
        route   $ setExtension "html.gz"
        compile gzipFileCompiler
