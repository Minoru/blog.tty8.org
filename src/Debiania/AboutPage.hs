{-# LANGUAGE OverloadedStrings #-}

module Debiania.AboutPage (
    aboutPageRules
) where

import Data.Monoid ((<>))

import Hakyll

import Debiania.Compilers
import Debiania.Settings

aboutPageRules :: Rules ()
aboutPageRules = do
    create ["about.markdown"] $ do
        route   $ setExtension "html"
        compile $ debianiaCompiler
          >>= loadAndApplyTemplate "templates/about.html" debianiaCtx
          >>= loadAndApplyTemplate
                "templates/default.html"
                (debianiaCtx <> constField "navbar-about" "Yep")
          >>= relativizeUrls

    create ["about.markdown"] $ version "gzipped" $ do
        route   $ setExtension "html.gz"
        compile gzipFileCompiler
