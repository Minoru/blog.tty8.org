{-# LANGUAGE OverloadedStrings #-}

module Debiania.Feeds (
    feedsRules
) where

import Control.Monad (filterM)
import Data.List (intersect)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

import qualified Data.Text as T

import Hakyll

import Debiania.Compilers
import Debiania.Context
import Debiania.Settings

feedsRules :: Rules ()
feedsRules = sequence_ [ createFeed a b c d e | (a,b,c) <- feeds, (d,e) <- feedCompilers ]
  where
    allContent = loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
          >>= mapM absolutizeUrls
    russianContent = allContent
          >>= filterLanguage "russian"
    englishContent = allContent
          >>= filterLanguage "english"
    linuxRussianContent = russianContent
          >>= filterTags ["debian", "linux"]

    feeds =
          [ ( "all",       allContent,          allFeedConfiguration          )
          , ( "russian",   russianContent,      russianFeedConfiguration      )
          , ( "english",   englishContent,      englishFeedConfiguration      )
          , ( "linux-rus", linuxRussianContent, linuxRussianFeedConfiguration )
          ]

    feedCompilers =
          [ ( "rss",  renderRss )
          , ( "atom", renderAtom )
          ]


-- | Abstracts away the bulk of writing the rule to generate a feed with
-- desirable settings
createFeed
  ::    T.Text
     -> Compiler [Item String]
     -> FeedConfiguration
     -> T.Text
     -> (    FeedConfiguration
          -> Context String
          -> [Item String]
          -> Compiler (Item String))
     -> Rules ()
createFeed name content conf extension compiler = do
  create [ feedpath ] $ do
    route     idRoute
    compile $ do
      content
        >>= fmap (take 10) . recentFirst
        >>= compiler
              conf
              (field "root" (\item -> do
                              let identifier = itemIdentifier item
                              published <- getItemUTC defaultTimeLocale identifier

                              httpsSwitchDate <-
                                parseTimeM
                                  False
                                  defaultTimeLocale
                                  "%FT%TZ"
                                  "2016-06-26T00:00:00Z"

                              tty8orgSwitchDate <-
                                parseTimeM
                                  False
                                  defaultTimeLocale
                                  "%FT%TZ"
                                  "2024-03-25T00:00:00Z"


                              if published >= tty8orgSwitchDate
                                then return rootUrl
                                else if published > httpsSwitchDate
                                  then return oldRootUrl
                                  else return oldOldRootUrl)
              `mappend`
              feedCtx)

  -- compressed version
  create [ feedpath ] $ version "gzipped" $ do
    route   $ customRoute $ (++ ".gz") . toFilePath
    compile gzipFileCompiler

  where feedpath = fromFilePath
          $ T.unpack
          $ T.concat [ "feeds/", name, ".", extension ]

-- | Return only items that have at least one of the specified tags
filterTags :: [String] -> [Item a] -> Compiler [Item a]
filterTags tags items = filterM (tagsIncludeOneOf tags) items

-- | Check if the given item has at least one of the specified tags
tagsIncludeOneOf :: [String] -> Item a -> Compiler Bool
tagsIncludeOneOf tags item = do
  let identifier = itemIdentifier item
  tags' <- getTags identifier
  return $ not $ null $ tags' `intersect` tags

-- | Return only items that are written in a specified language
filterLanguage :: String -> [Item a] -> Compiler [Item a]
filterLanguage language items = filterM (hasLanguage language) items

-- | Check if the given item is written in the specified language
hasLanguage :: String -> Item a -> Compiler Bool
hasLanguage language item = do
  let identifier = itemIdentifier item
  lang <- getMetadataField identifier "language"
  case lang of
    Just l  -> return $ l == language
    Nothing -> return False

absolutizeUrls :: Item String -> Compiler (Item String)
absolutizeUrls item = do
  itemRoute <- getRoute $ itemIdentifier item
  return $ case itemRoute of
    Nothing -> item
    Just _  -> fmap (relativizeUrlsWith rootUrl) item

feedCtx :: Context String
feedCtx =
    bodyField "description"
    <> postCtx
    <> defaultContext

{---- FEED CONFIGURATIONS ----}

-- There's a clear way to DRY up the code by reusing duplicate values, but I
-- rather won't: that makes it harder to understand what settings each feed has

allFeedConfiguration :: FeedConfiguration
allFeedConfiguration = FeedConfiguration
    { feedTitle = "Debiania"
    , feedDescription = "All posts"
    , feedAuthorName = "Alexander Batischev"
    , feedAuthorEmail = "eual.jp@gmail.com"
    , feedRoot = rootUrl
    }

russianFeedConfiguration :: FeedConfiguration
russianFeedConfiguration = FeedConfiguration
    { feedTitle = "Debiania"
    , feedDescription = "Посты на русском"
    , feedAuthorName = "Александр Батищев"
    , feedAuthorEmail = "eual.jp@gmail.com"
    , feedRoot = rootUrl
    }

englishFeedConfiguration :: FeedConfiguration
englishFeedConfiguration = FeedConfiguration
    { feedTitle = "Debiania"
    , feedDescription = "Posts in English only"
    , feedAuthorEmail = "eual.jp@gmail.com"
    , feedAuthorName = "Alexander Batischev"
    , feedRoot = rootUrl
    }

linuxRussianFeedConfiguration :: FeedConfiguration
linuxRussianFeedConfiguration = FeedConfiguration
    { feedTitle = "Debiania"
    , feedDescription = "О Linux"
    , feedAuthorName = "Александр Батищев"
    , feedAuthorEmail = "eual.jp@gmail.com"
    , feedRoot = rootUrl
    }
