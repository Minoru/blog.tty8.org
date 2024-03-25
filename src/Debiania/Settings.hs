module Debiania.Settings (
    rootUrl
  , oldRootUrl
  , oldOldRootUrl
  , debianiaConfig
) where

import Hakyll

rootUrl, oldRootUrl, oldOldRootUrl :: String
rootUrl = "https://blog.tty8.org"
oldRootUrl = "https://blog.debiania.in.ua"
oldOldRootUrl = "http://blog.debiania.in.ua"

debianiaConfig :: Configuration
debianiaConfig = defaultConfiguration {
  deployCommand =
    "rsync -rPvce ssh --chmod=ugo=rwX --no-times _site/ www-blog@blog.tty8.org:www/"
}
