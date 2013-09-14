{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM, filterM)
import Data.List (intersect)
import Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Text as T

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Build tags (will be used later on)
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

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

    create ["index.html"] $ do
        route     idRoute
        compile $ do
          posts <- fmap (take 8) . recentFirst =<< loadAll "posts/*"
          let ctx =    constField "title" "Home"
                    <> listField "posts" postCtx (return posts)
                    <> defaultContext

          makeItem ""
            >>= loadAndApplyTemplate "templates/index.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/post.html" (postCtx <> tagsCtx tags)
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

    -- Render About and Subscribe pages
    create ["about.markdown", "subscribe.markdown"] $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/about.html" defaultContext
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    -- Error 404 page is special in a way that it doesn't need URL
    -- relativization because it would be located in the webserver root
    create ["404.markdown"] $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/about.html" defaultContext
          >>= loadAndApplyTemplate "templates/default.html" defaultContext

    -- Render feeds
    -- TODO: look for 'description' field and use it instead (if present)
    let allContent = loadAllSnapshots "posts/*" "content"
    let russianContent = allContent
          >>= filterLanguage "russian"
    let englishContent = allContent
          >>= filterLanguage "english"
    let linuxRussianContent = russianContent
          >>= filterTags ["debian", "linux"]

    let feeds =
          [ ( "all",       allContent,          allFeedConfiguration          )
          , ( "russian",   russianContent,      russianFeedConfiguration      )
          , ( "english",   englishContent,      englishFeedConfiguration      )
          , ( "linux-rus", linuxRussianContent, linuxRussianFeedConfiguration )
          ]

    let feedCompilers =
          [ ( "rss",  renderRss )
          , ( "atom", renderAtom )
          ]

    sequence_
      [ createFeed a b c d e | (a,b,c) <- feeds, (d,e) <- feedCompilers ]

-- | Return only items that have at least one of the specified tags
filterTags :: [String] -> [Item a] -> Compiler [Item a]
filterTags tags items = filterM (hasTags tags) items

-- | Check if the given item has at least one of the specified tags
hasTags :: [String] -> Item a -> Compiler Bool
hasTags tags item = do
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
  metadata <- getMetadata identifier
  case (M.lookup "language" metadata) of
    Just l  -> return $ l == language
    Nothing -> return False

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
createFeed name content conf extension compiler =
  create [ feedpath ] $ do
    route     idRoute
    compile $ do
      content
        >>= fmap (take 10) . recentFirst
        >>= compiler conf feedCtx

  where feedpath = fromFilePath
          $ T.unpack
          $ T.concat [ "feeds/", name, ".", extension ]

{---- SETTINGS ----}

rootUrl = "http://debiania.in.ua"

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"
    <> dateField "datetime" "%Y-%m-%d"
    <> defaultContext

feedCtx :: Context String
feedCtx =
    bodyField "description"
    <> postCtx

tagsCtx :: Tags -> Context String
tagsCtx tags =
    tagsField "tags" tags
    <> defaultContext

{---- FEED CONFIGURATIONS ----}

-- There's a clear way to DRY up the code by reusing duplicate values, but I
-- rather won't: that makes it harder to understand what settings each feed has

allFeedConfiguration :: FeedConfiguration
allFeedConfiguration = FeedConfiguration
    { feedTitle = "Debiania, yet another Debian blog"
    , feedDescription = "All posts"
    , feedAuthorName = "Alexander Batischev"
    , feedAuthorEmail = "eual.jp@gmail.com"
    , feedRoot = rootUrl
    }

russianFeedConfiguration :: FeedConfiguration
russianFeedConfiguration = FeedConfiguration
    { feedTitle = "Debiania, ещё один блог о Debian"
    , feedDescription = "Посты на русском"
    , feedAuthorName = "Александр Батищев"
    , feedAuthorEmail = "eual.jp@gmail.com"
    , feedRoot = rootUrl
    }

englishFeedConfiguration :: FeedConfiguration
englishFeedConfiguration = FeedConfiguration
    { feedTitle = "Debiania, yet another Debian blog"
    , feedDescription = "Posts in English only"
    , feedAuthorEmail = "eual.jp@gmail.com"
    , feedAuthorName = "Alexander Batischev"
    , feedRoot = rootUrl
    }

linuxRussianFeedConfiguration :: FeedConfiguration
linuxRussianFeedConfiguration = FeedConfiguration
    { feedTitle = "Debiania, ещё один блог о Debian"
    , feedDescription = "О Linux"
    , feedAuthorName = "Александр Батищев"
    , feedAuthorEmail = "eual.jp@gmail.com"
    , feedRoot = rootUrl
    }

