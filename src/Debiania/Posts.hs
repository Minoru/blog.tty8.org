{-# LANGUAGE OverloadedStrings #-}

module Debiania.Posts (
    postsRules
) where

import Data.Monoid ((<>))
import Network.HTTP.Base (urlEncode)

import Hakyll

import Debiania.Compilers
import Debiania.Context

postsRules :: Rules ()
postsRules = do
    -- Build tags (will be used later on)
    tags <- buildTags ("posts/*" .&&. hasNoVersion) (fromCapture "tags/*.html")

    match "posts/*" $ do
        route   $ setExtension "html"
        compile $ do
          debianiaCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate
                  "templates/post.html"
                  (mconcat [ urlEncodedTitleCtx
                           , postCtx
                           , tagsField "tags" tags
                           , defaultContext
                           ])
            >>= loadAndApplyTemplate
                  "templates/default.html"
                  (rootUrlCtx <> defaultContext)
            >>= relativizeUrls

    match "posts/*" $ version "gzipped" $ do
        route   $ setExtension "html.gz"
        compile gzipFileCompiler

urlEncodedTitleCtx :: Context String
urlEncodedTitleCtx =
  field
    "urlEncodedTitle"
    (\item -> do
      let identifier = itemIdentifier item
      title <- getMetadataField identifier "title"
      case title of
        Nothing -> return ""
        Just t  -> return $ urlEncode t
    )
