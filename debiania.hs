{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Images and miscellaneous files
    match ( "images/*" .||. "misc/*" .||. "robots.txt" ) $ do
        route   idRoute
        compile copyFileCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    -- Render Archives page
    create ["posts.html"] $ do
        route     idRoute
        compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let ctx =    constField "title" "Archives"
                    <> listField "posts" postCtx (return posts)
                    <> defaultContext

          makeItem ""
            >>= loadAndApplyTemplate "templates/archives.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

{---- SETTINGS ----}

rootUrl = "http://debiania.in.ua"

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
          <> dateField "datetime" "%Y-%m-%d"
          <> defaultContext

