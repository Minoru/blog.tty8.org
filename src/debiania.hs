{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM)
import Data.Function (on)
import Data.List (sort, groupBy, sortOn)
import Data.Monoid ((<>))
import Data.String.Utils (split)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.HTTP.Base (urlEncode)
import Text.Pandoc.Options (WriterOptions(writerHTMLMathMethod),
    HTMLMathMethod(MathJax))

import qualified Data.HashMap.Strict as M

import Hakyll

import Debiania.Compilers
import Debiania.Feeds
import Debiania.Posts
import Debiania.Settings

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
                     let identifier = itemIdentifier post
                     time <- getItemUTC defaultTimeLocale identifier
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

          makeItem ("" :: String)
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

    feedsRules

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
    emptyItem <- makeItem ("" :: String)
    newItem <- loadAndApplyTemplate
                 "templates/break.html"
                 (constField "before" before <> constField "after" after)
                 emptyItem
    let newBody = itemBody newItem
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
toMapElem ([]:xs) = toMapElem xs
toMapElem (((a, b):as):xs) = (a, b : map snd as) : toMapElem xs

{---- SETTINGS ----}

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

tagsCtx :: Tags -> Context String
tagsCtx tags =
    tagsField "tags" tags
    <> debianiaCtx
