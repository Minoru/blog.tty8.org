{-# LANGUAGE OverloadedStrings #-}

module Debiania.IndexPage (
    indexPageRules
) where

import Data.Monoid ((<>))

import Hakyll

import Debiania.Compilers
import Debiania.Context
import Debiania.Posts

indexPageRules :: Rules ()
indexPageRules = do
    create ["index.html"] $ do
        route     idRoute
        compile $ do
          posts <- fmap (take 8) . recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
          let ctx =    constField "title" "Home"
                    <> constField "navbar-home" "Yep"
                    <> listField
                         "posts"
                         (postCtx <> defaultContext)
                         (return posts)
                    <> rootUrlCtx
                    <> defaultContext

          makeItem ""
            >>= loadAndApplyTemplate "templates/index.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    create ["index.html"] $ version "gzipped" $ do
        route   $ setExtension "html.gz"
        compile gzipFileCompiler
