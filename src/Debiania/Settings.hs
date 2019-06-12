module Debiania.Settings (
    rootUrl
  , oldRootUrl
  , rootUrlCtx
  , debianiaConfig
) where

import Hakyll

rootUrl, oldRootUrl :: String
rootUrl = "https://blog.debiania.in.ua"
oldRootUrl = "http://blog.debiania.in.ua"

rootUrlCtx :: Context String
rootUrlCtx = constField "root" rootUrl

debianiaConfig :: Configuration
debianiaConfig = defaultConfiguration {
  deployCommand =
    "rsync -rPvce ssh --chmod=ugo=rwX --no-times _site/ www-blog@theke.debiania.in.ua:"
}
