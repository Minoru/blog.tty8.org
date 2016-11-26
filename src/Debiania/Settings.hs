module Debiania.Settings (
    rootUrl
  , oldRootUrl
  , debianiaCtx
) where

import Data.Monoid ((<>))

import Hakyll

rootUrl, oldRootUrl :: String
rootUrl = "https://blog.debiania.in.ua"
oldRootUrl = "http://blog.debiania.in.ua"

debianiaCtx :: Context String
debianiaCtx =
       constField "root" rootUrl
    <> defaultContext
