module Debiania.Settings (
    rootUrl
  , oldRootUrl
  , debianiaConfig
) where

import Hakyll

rootUrl, oldRootUrl :: String
rootUrl = "https://blog.debiania.in.ua"
oldRootUrl = "http://blog.debiania.in.ua"

debianiaConfig :: Configuration
debianiaConfig = defaultConfiguration {
  deployCommand =
    "rsync -rPvce ssh --chmod=ugo=rwX --no-times _site/ www-blog@theke.debiania.in.ua:"
}
