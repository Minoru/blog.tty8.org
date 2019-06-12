{-# LANGUAGE OverloadedStrings #-}

module Debiania.Sitemap (
    sitemapRules
) where

import Data.Monoid ((<>))

import Hakyll

import Debiania.Compilers
import Debiania.Posts
import Debiania.Settings

sitemapRules :: Rules ()
sitemapRules = do
    create ["sitemap.xml"] $ do
       route   idRoute
       compile $ do
         posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
         let sitemapCtx =    listField
                               "posts"
                               (postCtx <> defaultContext)
                               (return posts)
                          <> rootUrlCtx
                          <> defaultContext
         makeItem ""
          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    create ["sitemap.xml"] $ version "gzipped" $ do
        route   $ setExtension "xml.gz"
        compile gzipFileCompiler
