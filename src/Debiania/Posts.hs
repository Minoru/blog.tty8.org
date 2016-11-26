module Debiania.Posts (
    postCtx
) where

import Data.Monoid ((<>))

import Hakyll

import Debiania.Settings

postCtx :: Context String
postCtx =
       dateField "date" "%e %B %Y"
    <> dateField "datetime" "%Y-%m-%d"
    <> debianiaCtx
