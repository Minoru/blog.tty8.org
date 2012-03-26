{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (|||), (&&&), first, second, arr)
import Data.Monoid (mempty, mconcat)
import Data.List (sort, elem)
import qualified Data.Text as T

import Hakyll

rootUrl = ""

main :: IO ()
main = hakyll $ do
    -- Compress CSS
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Images
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    -- Miscellaneous files
    match "misc/*" $ do
        route idRoute
        compile copyFileCompiler

    -- Render posts
    match "posts/*" $ do
        route $ setExtension ".html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> arr (renderDateField "datetime" "%Y-%m-%d" "")
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Render posts list
    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "Archives")
        >>> requireAllA "posts/*" addPostList
        >>> applyTemplateCompiler "templates/archives.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> requireAllA "posts/*" (second (arr $ take 8 . recentFirst)
                                   >>> addPostList)
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    match "about.markdown" $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>> arr (setField "title" "About")
            >>> applyTemplateCompiler "templates/about.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    match "subscribe.markdown" $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>> arr (setField "title" "Subscribe")
            >>> applyTemplateCompiler "templates/about.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    match "404.markdown" $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>> arr (setField "title" "404")
            >>> applyTemplateCompiler "templates/about.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Render feeds
    -- All posts
    match "all.rss" $ route idRoute
    create "all.rss" $ allPosts >>> renderRss allFeedConfiguration

    match "all.atom" $ route idRoute
    create "all.atom" $ allPosts >>> renderAtom allFeedConfiguration

    -- Russian only
    match "russian.rss" $ route idRoute
    create "russian.rss" $ russianPosts >>> renderRss russianFeedConfiguration

    match "russian.atom" $ route idRoute
    create "russian.atom" $ russianPosts >>> renderAtom russianFeedConfiguration

    -- English only
    match "english.rss" $ route idRoute
    create "english.rss" $ englishPosts >>> renderRss englishFeedConfiguration

    match "english.atom" $ route idRoute
    create "english.atom" $ englishPosts >>> renderAtom englishFeedConfiguration

    -- Debian-related, russian
    match "debian-rus.rss" $ route idRoute
    create "debian-rus.rss" $ debianRussianPosts
        >>> renderRss debianRussianFeedConfiguration

    match "debian-rus.atom" $ route idRoute
    create "debian-rus.atom" $ debianRussianPosts
        >>> renderAtom debianRussianFeedConfiguration

    -- Linux-related, russian - feed for runix.org
    match "linux-rus.rss" $ route idRoute
    create "linux-rus.rss" $ linuxRussianPosts
        >>> renderRss linuxRussianFeedConfiguration

    match "linux-rus.atom" $ route idRoute
    create "linux-rus.atom" $ linuxRussianPosts
        >>> renderAtom linuxRussianFeedConfiguration

    -- Read templates
    match "templates/*" $ compile templateCompiler

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr recentFirst
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

hasDescription :: Page a -> Bool
hasDescription = (/= "") . getField "description"

pageHasDescription :: Compiler (Page a) (Either (Page a) (Page a))
pageHasDescription = arr (\p -> if hasDescription p then Right p else Left p)

isRussian :: Page String -> Bool
isRussian p = field || heuristics
  where
    field = any (\l -> l == "ru" || l == "rus") [ getField "language" p
                                                , getField "lang" p ]
    heuristics = counted >= (T.length text `div` 20)
    counted = sum $ map snd $ filter ((`elem` russian) . fst) $ countChars text
    countChars = map (\t -> (T.head t, T.length t)) . T.group
    text = T.toCaseFold $ T.pack $ sort $ pageBody p

    russian = "йцукенгшщзхъфывапролджэячсмитьбю"

isDebianRelated = ("debian" `elem`) . map T.unpack . T.splitOn " ," . T.pack
    . getField "category"

isLinuxRelated = ("linux" `elem`) . map T.unpack . T.splitOn " ," . T.pack
    . getField "category"

genFeedEntries :: Compiler [Page String] [Page String]
genFeedEntries = mapCompiler $ pageHasDescription >>>
  ((arr (pageBody &&& id)
    >>> first (unixFilter "sed" ["-n", "/<article[^>]*>/,/<\\/article>/p"])
    >>> arr (\(b, m) -> setField "description" b m))
  |||
    id)

allPosts :: Compiler () [Page String]
allPosts = requireAll_ "posts/*" >>> genFeedEntries

russianPosts = requireAll_ "posts/*"
    >>> arr (filter isRussian)
    >>> genFeedEntries

englishPosts = requireAll_ "posts/*"
    >>> arr (filter (not . isRussian))
    >>> genFeedEntries

debianRussianPosts = requireAll_ "posts/*"
    >>> arr (filter isDebianRelated)
    >>> arr (filter isRussian)
    >>> genFeedEntries

linuxRussianPosts = requireAll_ "posts/*"
    >>> arr (filter (\p -> isLinuxRelated p || isDebianRelated p))
    >>> arr (filter isRussian)
    >>> genFeedEntries

allFeedConfiguration :: FeedConfiguration
allFeedConfiguration = FeedConfiguration
    { feedTitle = "Debiania, yet another Debian blog"
    , feedDescription = "All posts"
    , feedAuthorName = "Alexander Batischev"
    , feedRoot = rootUrl
    }

russianFeedConfiguration :: FeedConfiguration
russianFeedConfiguration = FeedConfiguration
    { feedTitle = "Debiania, ещё один блог о Debian"
    , feedDescription = "Посты на русском"
    , feedAuthorName = "Александр Батищев"
    , feedRoot = rootUrl
    }

englishFeedConfiguration :: FeedConfiguration
englishFeedConfiguration = FeedConfiguration
    { feedTitle = "Debiania, yet another Debian blog"
    , feedDescription = "Posts in English only"
    , feedAuthorName = "Alexander Batischev"
    , feedRoot = rootUrl
    }

debianRussianFeedConfiguration :: FeedConfiguration
debianRussianFeedConfiguration = FeedConfiguration
    { feedTitle = "Debiania, ещё один блог о Debian"
    , feedDescription = "О Debian"
    , feedAuthorName = "Александр Батищев"
    , feedRoot = rootUrl
    }

linuxRussianFeedConfiguration :: FeedConfiguration
linuxRussianFeedConfiguration = FeedConfiguration
    { feedTitle = "Debiania, ещё один блог о Debian"
    , feedDescription = "О Linux"
    , feedAuthorName = "Александр Батищев"
    , feedRoot = rootUrl
    }

