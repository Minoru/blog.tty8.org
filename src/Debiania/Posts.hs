{-# LANGUAGE OverloadedStrings #-}

module Debiania.Posts (
    postsRules
) where

import Data.List (intersperse, sort)
import Control.Monad (liftM, when)
import Network.HTTP.Base (urlEncode)
import Text.Blaze.Html (toHtml, toValue, (!))

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import Hakyll

import Debiania.Compilers
import Debiania.Context

postsRules :: Rules ()
postsRules = do
    tags <- buildTags ("posts/*" .&&. hasNoVersion) (fromCapture "tags/*.html")

    match "posts/*" $ do
        route   $ setExtension "html"
        compile $ do
          postId <- getUnderlying
          postTags <- getTags postId
          when (null postTags) $ do
              error $ concat [toFilePath postId, " contains no tags, exiting"]

          debianiaCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate
                  "templates/post.html"
                  (mconcat [ urlEncodedTitleCtx
                           , sortedTagsCtx tags
                           , postCtx
                           , defaultContext
                           ])
            >>= loadAndApplyTemplate
                  "templates/default.html"
                  (rootUrlCtx <> defaultContext)
            >>= relativizeUrls

    match "posts/*" $ version "gzipped" $ do
        route   $ setExtension "html.gz"
        compile gzipFileCompiler

    tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            yearsCtx <- makeYearsCtx posts

            let ctx = constField "tag" tag
                      <> yearsCtx
                      <> rootUrlCtx
                      <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archives.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    tagsRules tags $ \_tag _pattern -> version "gzipped" $ do
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

-- | "tags" field with item's tags, sorted alphabetically.
sortedTagsCtx :: Tags -> Context String
sortedTagsCtx tags =
  tagsFieldWith
   (liftM sort . getTags)
   simpleRenderLink
   (mconcat . intersperse ", ")
   "tags"
   tags

-- | Render one tag link
--
-- Lifted straight from Hakyll.Web.Tags.
simpleRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderLink _   Nothing         = Nothing
simpleRenderLink tag (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag
