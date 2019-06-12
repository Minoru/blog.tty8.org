{-# LANGUAGE OverloadedStrings #-}

module Debiania.Context (
    makeYearsCtx
  , rootUrlCtx
  , postCtx
) where

import Control.Monad (forM)
import Data.List (sort)
import Data.Monoid ((<>))
import Data.Time.Format (defaultTimeLocale, formatTime)

import qualified Data.HashMap.Strict as M

import Hakyll

import Debiania.Settings

-- | "root" field containing site's root URL.
rootUrlCtx :: Context String
rootUrlCtx = constField "root" rootUrl

-- | Post's "date" and "datetime" fields, plus `rootUrlCtx`.
postCtx :: Context String
postCtx =
       dateField "date" "%e %B %Y"
    <> dateField "datetime" "%Y-%m-%d"
    <> rootUrlCtx

-- | "years" list, with "year" and "posts" fields containing all posts
-- published in a given year.
--
-- Result only includes `posts`. Handy for Archives (where `posts = loadAll
-- "posts/*"`) and tags (where `posts = loadAll tagPattern`).
makeYearsCtx :: [Item String] -> Compiler (Context String)
makeYearsCtx posts = do
  -- Pair each post with the year it was posted on
  years <- forM posts $ \post -> do
             let identifier = itemIdentifier post
             time <- getItemUTC defaultTimeLocale identifier
             let year = formatTime defaultTimeLocale "%Y" time
             return (year, post)

  -- Convert the list of pairs into a map, from year to a list of posts
  -- that were posted that year
  let postsByYear =
        M.fromListWith
          (++)
          [ (year, [post]) | (year, post) <- years]

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

  return yearsCtx
