{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (Alternative (..))
import Control.Arrow ((***))
import Control.Monad (liftM, filterM, forM, msum)
import Data.Function (on)
import Data.List (intersect, sort, sortBy, groupBy, sortOn, intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.String.Utils (split)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM, formatTime)
import Network.HTTP.Base (urlEncode)
import System.FilePath (takeFileName)
import Text.Pandoc.Options (WriterOptions(writerHTMLMathMethod),
    HTMLMathMethod(MathJax), def)

import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Hakyll

config :: Configuration
config = defaultConfiguration {
  deployCommand = "rsync -rPv --chmod=ugo=rwX _site/ www-blog@theke.debiania.in.ua:"
}

main :: IO ()
main = hakyllWith config $ do
    -- Build tags (will be used later on)
    tags <- buildTags ("posts/*" .&&. hasNoVersion) (fromCapture "tags/*.html")

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

    -- MathJax stuff
    match ( "MathJax/MathJax.js" .||.
            "MathJax/extensions/**" .||.
            "MathJax/fonts/**" .||.
            "MathJax/jax/**" .||.
            "MathJax/localization/**") $ do
        route   idRoute
        compile copyFileCompiler

    create ["index.html"] $ do
        route     idRoute
        compile $ do
          posts <- fmap (take 8) . recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
          let ctx =    constField "title" "Home"
                    <> constField "navbar-home" "Yep"
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
          debianiaCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate
                  "templates/post.html"
                  (mconcat [ urlEncodedTitleCtx
                           , postCtx
                           , tagsCtx tags
                           ])
            >>= loadAndApplyTemplate "templates/default.html" debianiaCtx
            >>= relativizeUrls

    -- Render Archives page
    create ["posts.markdown"] $ do
        route   $ setExtension "html"
        compile $ do
          -- On this page, we're listing posts by year (in reverse
          -- chronological order.

          -- First of all, load all the posts
          posts <- loadAll ("posts/*" .&&. hasNoVersion)
          -- Now pair each post with the year it was posted on
          years <- forM posts $ \post -> do
                     let id = itemIdentifier post
                     time <- getItemUTC defaultTimeLocale id
                     let year = formatTime defaultTimeLocale "%Y" time
                     return (year, post)

          -- Convert the list of pairs into a map, from year to a list of posts
          -- that were posted that year
          let postsByYear = M.fromList
                          $ toMapElem
                          $ groupBy ((==) `on` fst)
                          $ reverse
                          $ sortOn fst years

          -- Here's how we're going to achieve our goal. First, we create
          -- a list of items, one item per year. The body of the item contains
          -- a single string--the year. That list is available in `yearsCtx`
          -- context.
          --
          -- In the template, we will walk over that list and evaluate each
          -- item in `yearCtx` context. This will give us two fields: `year`,
          -- which is a string with the year in it; and `posts`, which is
          -- a list of posts that were posted that year.
          --
          -- When we're walking over `posts` list, we're evaluating it in the
          -- `postCtx` context, but with a twitch: `date` field will look like
          -- "Jul 01" (with non-breaking space!).
          let yearCtx =  field "year" (return . itemBody)
                      <> listFieldWith
                            "posts"
                            -- format date as "Jul 01", "Mar 21" etc.
                            (  dateField "date" "%b&nbsp;%d"
                            <> postCtx)
                            (\item -> do let year = itemBody item
                                         return $ postsByYear M.! year)

          let yearsDescending = reverse $ sort $ M.keys postsByYear
          let yearsCtx = listField "years" yearCtx (mapM makeItem yearsDescending)

          makeItem ""
            >>= loadAndApplyTemplate "templates/archives.html" yearsCtx
            >>= loadAndApplyTemplate
                  "templates/default.html"
                  (  constField "title" "Archives"
                  <> constField "navbar-archives" "Yep"
                  <> debianiaCtx)
            >>= relativizeUrls

    -- Render About, Subscribe and 404 pages
    create ["about.markdown"] $ do
        route   $ setExtension "html"
        compile $ debianiaCompiler
          >>= loadAndApplyTemplate "templates/about.html" debianiaCtx
          >>= loadAndApplyTemplate
                "templates/default.html"
                (debianiaCtx <> constField "navbar-about" "Yep")
          >>= relativizeUrls

    create ["subscribe.markdown"] $ do
        route   $ setExtension "html"
        compile $ debianiaCompiler
          >>= loadAndApplyTemplate "templates/about.html" debianiaCtx
          >>= loadAndApplyTemplate
                "templates/default.html"
                (debianiaCtx <> constField "navbar-subscribe" "Yep")
          >>= relativizeUrls

    create ["404.markdown"] $ do
        route   $ setExtension "html"
        compile $ debianiaCompiler
          >>= loadAndApplyTemplate "templates/about.html" debianiaCtx
          >>= loadAndApplyTemplate "templates/default.html" debianiaCtx
          >>= relativizeUrls

    create ["sitemap.xml"] $ do
       route   idRoute
       compile $ do
         posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
         let sitemapCtx =    listField "posts" postCtx (return posts)
                          <> debianiaCtx
         makeItem ""
          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    create ["sitemap.xml"] $ version "gzipped" $ do
        route   $ setExtension "xml.gz"
        compile gzipFileCompiler

    create ["css/debiania.css.gz"] $ do
        route idRoute
        compile $ do
          css <- load "css/debiania.css"
          makeItem (itemBody css)
            >>= gzip

    match "posts/*" $ version "gzipped" $ do
        route   $ setExtension "html.gz"
        compile gzipFileCompiler

    create ["about.markdown"
           , "subscribe.markdown"
           , "404.markdown"
           , "posts.markdown"
           , "index.html"]
      $ version "gzipped" $ do
        route   $ setExtension "html.gz"
        compile gzipFileCompiler

    match "images/*.svg" $ version "gzipped" $ do
        route   $ setExtension "svg.gz"
        compile $ do
          getResourceBody
            >>= gzip

    -- Render feeds
    let allContent = loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
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

  -- compressed version
  create [ feedpath ] $ version "gzipped" $ do
    route   $ customRoute $ (++ ".gz") . toFilePath
    compile gzipFileCompiler

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
  >>= renderPandocWith
        defaultHakyllReaderOptions
        -- The empty string is path to mathjax.js. We don't want Pandoc to
        -- embed it in output for us as we already do that in Hakyll templates.
        (defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "" })

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

-- | Prepare a list to be used as a base to constuct a Map.
--
-- Example input:
--
--    [[(1, "hello"), (1, "hi"), (1, "hey")], [(2, "bye"), (2, "see ya")]]
--
-- ...will result in:
--
--    [(1, ["hello", "hi", "hey"]), (2, ["bye", "see ya"])]
toMapElem :: [[(a, b)]] -> [(a, [b])]
toMapElem [] = []
toMapElem (((a, b):as):xs) = (a, b : map snd as) : toMapElem xs

gzip :: Item String -> Compiler (Item LBS.ByteString)
gzip = withItemBody
         (unixFilterLBS
           "7z"
           [ "a"      -- create archive
           , "dummy"  -- archive's filename (won't be used)
           , "-tgzip" -- archive format
           , "-mx9"   -- m-m-maximum compression
           , "-si"    -- read data from stdin
           , "-so"    -- write archive to stdout
           ]
         . LBS.fromStrict
         . TE.encodeUtf8
         . T.pack)

gzipFileCompiler :: Compiler (Item LBS.ByteString)
gzipFileCompiler = do id <- getUnderlying
                      body <- loadBody (setVersion Nothing id)
                      makeItem body
                        >>= gzip

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
       dateField "date" "%e %B %Y"
    <> dateField "datetime" "%Y-%m-%d"
    <> debianiaCtx

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

