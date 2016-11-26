module Debiania.Settings (
    rootUrl
  , oldRootUrl
  , debianiaCtx
  , debianiaConfig
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

debianiaConfig :: Configuration
debianiaConfig = defaultConfiguration {
  deployCommand =
    "rsync -rPv --chmod=ugo=rwX _site/ www-blog@theke.debiania.in.ua:"
}
