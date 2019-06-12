module Debiania.Context (
    rootUrlCtx
  , postCtx
) where

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
