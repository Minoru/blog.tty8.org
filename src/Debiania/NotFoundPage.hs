{-# LANGUAGE OverloadedStrings #-}

module Debiania.NotFoundPage (
    notFoundPageRules
) where

import Hakyll

import Debiania.Compilers
import Debiania.Context

notFoundPageRules :: Rules ()
notFoundPageRules = do
    let ctx = rootUrlCtx <> defaultContext

    create ["404.markdown"] $ do
        route   $ setExtension "html"
        compile $ debianiaCompiler
          >>= loadAndApplyTemplate "templates/about.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          -- do not relativize URLs since this page can be shown anywhere, e.g.
          -- from /images/ or /posts/

    create ["404.markdown"] $ version "gzipped" $ do
        route   $ setExtension "html.gz"
        compile gzipFileCompiler
