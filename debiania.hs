{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (Alternative (..))
import Control.Arrow ((***))
import Control.Monad (liftM, filterM, msum)
import Data.List (intersect, sortBy, intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.String.Utils (split)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM, formatTime)
import Network.HTTP.Base (urlEncode)
import System.FilePath (takeFileName)

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Hakyll

config :: Configuration
config = defaultConfiguration {
  deployCommand = "rsync -rPv --chmod=ugo=rwX _site/ www-blog@theke.debiania.in.ua:"
}

main :: IO ()
main = hakyllWith config $ do
    -- Build tags (will be used later on)
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    let withDate (identifier, m) =
          let fn = takeFileName $ toFilePath identifier
              datetime = parseTimeM True defaultTimeLocale "%Y-%m-%d" $
                         intercalate "-" $
                         take 3 $
                         splitAll "-" fn
              date = fromMaybe "" $
                       msum [ M.lookup "published" m
                            , M.lookup "date" m
                            , fmap
                                (formatTime defaultTimeLocale "%F")
                                (datetime :: Maybe UTCTime)
                            ]
          in (identifier, date)

    postsMetadata <-
            map fst . sortBy (comparing snd) . map withDate
        <$> getAllMetadata "posts/*"

    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Compress CSS
    match "css/*" $ do
        -- No `route` rule means these files won't be present in generated
        -- site. The point of this whole block is to compress the files and put
        -- them into the cache so that later on, we can use `loadBody` to take
        -- them out and generate debiania.css out of them.
        compile compressCssCompiler

    create ["css/debiania.css"] $ do
        route idRoute
        compile $ do
          styles <- mapM
                      (loadBody . fromFilePath . ("css/"++))
                      [ "normalize.css"
                      , "syntax.css"
                      , "default.css"
                      , "desktop.css"
                      , "mobile.css"]
          makeItem (concat (styles :: [String]))

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
                    <> debianiaCtx

          makeItem ""
            >>= loadAndApplyTemplate "templates/index.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension "html"
        compile $ do
          prevNextCtx <- genPrevNextCtx postsMetadata =<< getUnderlying

          debianiaCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate
                  "templates/post.html"
                  (mconcat [ urlEncodedTitleCtx
                           , postCtx
                           , tagsCtx tags
                           , prevNextCtx
                           ])
            >>= loadAndApplyTemplate "templates/default.html" debianiaCtx
            >>= relativizeUrls

    -- Render Archives page
    create ["posts.html"] $ do
        route     idRoute
        compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let ctx =    constField "title" "Archives"
                    <> listField "posts" postCtx (return posts)
                    <> debianiaCtx

          makeItem ""
            >>= loadAndApplyTemplate "templates/archives.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    -- Render About and Subscribe pages
    create ["about.markdown", "subscribe.markdown"] $ do
        route   $ setExtension "html"
        compile $ debianiaCompiler
          >>= loadAndApplyTemplate "templates/about.html" debianiaCtx
          >>= loadAndApplyTemplate "templates/default.html" debianiaCtx
          >>= relativizeUrls

    -- Error 404 page is special in a way that it doesn't need URL
    -- relativization because it would be located in the webserver root
    create ["404.markdown"] $ do
        route   $ setExtension "html"
        compile $ debianiaCompiler
          >>= loadAndApplyTemplate "templates/about.html" debianiaCtx
          >>= loadAndApplyTemplate "templates/default.html" debianiaCtx
          >>= relativizeUrls

    create ["sitemap.xml"] $ do
       route   idRoute
       compile $ do
         posts <- recentFirst =<< loadAll "posts/*"
         let sitemapCtx =    listField "posts" postCtx (return posts)
                          <> debianiaCtx
         makeItem ""
          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    -- Render feeds
    let allContent = loadAllSnapshots "posts/*" "content"
          >>= mapM absolutizeUrls
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

absolutizeUrls :: Item String -> Compiler (Item String)
absolutizeUrls item = do
  route <- getRoute $ itemIdentifier item
  return $ case route of
    Nothing -> item
    Just r  -> fmap (relativizeUrlsWith rootUrl) item

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
  lang <- getMetadataField identifier "language"
  case lang of
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
        >>= compiler
              conf
              (field "root" (\item -> do
                              let id = itemIdentifier item
                              published <- getItemUTC defaultTimeLocale id

                              httpsSwitchDate <-
                                parseTimeM
                                  False
                                  defaultTimeLocale
                                  "%FT%TZ"
                                  "2016-06-26T00:00:00Z"

                              if published > httpsSwitchDate
                                then return rootUrl
                                else return oldRootUrl)
              `mappend`
              feedCtx)

  where feedpath = fromFilePath
          $ T.unpack
          $ T.concat [ "feeds/", name, ".", extension ]

-- | Essentially a @pandocCompiler@, but with some preprocessing:
--
-- * @$break$@ on a separate line will be replaced by a section break image.
debianiaCompiler :: Compiler (Item String)
debianiaCompiler =
      getResourceBody
  >>= withItemBody (go . split "\n\n$break$\n\n")
  >>= renderPandoc

  where
  go :: [String] -> Compiler String
  go [] = return ""
  go [single] = return single
  go (before:after:rest) = do
    empty <- makeItem ""
    new <- loadAndApplyTemplate
             "templates/break.html"
             (constField "before" before <> constField "after" after)
             empty
    let newBody = itemBody new
    go (newBody:rest)

-- | Given a chronologically ordered list of posts' identifiers, find previous
-- and next post for a given one.
getPrevNextPosts ::
       [Identifier]
    -> Identifier
    -> (Maybe Identifier, Maybe Identifier)
getPrevNextPosts (id1 : rest@(id2:id3:_)) id
  | id == id1 = (Nothing, Just id2)
  | id > id2  = getPrevNextPosts rest id
  | id == id2 = (Just id1, Just id3)
  | otherwise = (Nothing, Nothing)
getPrevNextPosts [id1, id2] id
  | id == id1 = (Nothing, Just id2)
  | id == id2 = (Just id1, Nothing)
  | otherwise = (Nothing, Nothing)
getPrevNextPosts _ _ = (Nothing, Nothing)

{---- SETTINGS ----}

rootUrl = "https://blog.debiania.in.ua"
oldRootUrl = "http://blog.debiania.in.ua"

debianiaCtx :: Context String
debianiaCtx =
       constField "root" rootUrl
    <> defaultContext

urlEncodedTitleCtx :: Context String
urlEncodedTitleCtx =
  field
    "urlEncodedTitle"
    (\item -> do
      identifier <- getUnderlying
      title <- getMetadataField identifier "title"
      case title of
        Nothing -> return ""
        Just t  -> return $ urlEncode t
    )

postCtx :: Context String
postCtx =
       dateField "date" "%B %e, %Y"
    <> dateField "datetime" "%Y-%m-%d"
    <> debianiaCtx

genPrevNextCtx :: [Identifier] -> Identifier -> Compiler (Context String)
genPrevNextCtx posts identifier = do
    let (prev_url, next_url) = getPrevNextPosts posts identifier

    let getRoute' = maybe (return Nothing) getRoute
    prevUrl <- getRoute' prev_url
    nextUrl <- getRoute' next_url

    let genField url' field =
          case url' of
            -- getRoute returned a path relative to the site root, but without
            -- a slash at the beginning. We have to fix that.
            Just url -> [ constField field ("/" ++ url) ]
            Nothing  -> []

    return $ mconcat $ concat [
        genField prevUrl "prev_post_url"
      , genField nextUrl "next_post_url"
      ]

feedCtx :: Context String
feedCtx =
    bodyField "description"
    <> postCtx

tagsCtx :: Tags -> Context String
tagsCtx tags =
    tagsField "tags" tags
    <> debianiaCtx

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

