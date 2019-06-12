{-# LANGUAGE OverloadedStrings #-}

module Debiania.Archives (
    archivesRules
) where

import Control.Monad (forM)
import Data.Function (on)
import Data.List (sort, groupBy, sortOn)
import Data.Monoid ((<>))
import Data.Time.Format (defaultTimeLocale, formatTime)

import qualified Data.HashMap.Strict as M

import Hakyll

import Debiania.Compilers
import Debiania.Context
import Debiania.Posts

archivesRules :: Rules ()
archivesRules = do
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
                            <> postCtx
                            <> defaultContext)
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
                  <> rootUrlCtx
                  <> defaultContext)
            >>= relativizeUrls

    create ["posts.markdown"] $ version "gzipped" $ do
        route   $ setExtension "html.gz"
        compile gzipFileCompiler

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
