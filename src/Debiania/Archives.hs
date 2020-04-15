{-# LANGUAGE OverloadedStrings #-}

module Debiania.Archives (
    archivesRules
) where

import Hakyll

import Debiania.Compilers
import Debiania.Context

archivesRules :: Rules ()
archivesRules = do
    create ["posts.markdown"] $ do
        route   $ setExtension "html"
        compile $ do
          -- First of all, load all the posts
          posts <- loadAll ("posts/*" .&&. hasNoVersion)

          yearsCtx <- makeYearsCtx posts

          makeItem ("" :: String)
            >>= loadAndApplyTemplate "templates/archives.html" yearsCtx
            >>= loadAndApplyTemplate
                  "templates/default.html"
                  (  constField "title" "Archives"
                  <> constField "navbar-archives" "Yep"
                  <> rootUrlCtx
                  <> defaultContext)
            >>= relativizeUrls

    create ["posts.markdown"] $ version "gzipped" $ do
        route   $ setExtension "html.gz"
        compile gzipFileCompiler
