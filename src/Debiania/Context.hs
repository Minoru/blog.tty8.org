module Debiania.Context (
    rootUrlCtx
) where

import Hakyll

import Debiania.Settings

-- | "root" field containing site's root URL.
rootUrlCtx :: Context String
rootUrlCtx = constField "root" rootUrl
