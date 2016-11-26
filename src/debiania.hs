{-# LANGUAGE OverloadedStrings #-}

import Hakyll

import Debiania.AboutPage
import Debiania.Archives
import Debiania.Assets
import Debiania.CSS
import Debiania.Feeds
import Debiania.IndexPage
import Debiania.NotFoundPage
import Debiania.Posts
import Debiania.Sitemap
import Debiania.SubscribePage

config :: Configuration
config = defaultConfiguration {
  deployCommand = "rsync -rPv --chmod=ugo=rwX _site/ www-blog@theke.debiania.in.ua:"
}

main :: IO ()
main = hakyllWith config $ do
    -- Read templates
    match "templates/*" $ compile templateCompiler

    cssRules
    assetsRules
    indexPageRules
    postsRules
    archivesRules
    aboutPageRules
    subscribePageRules
    sitemapRules
    notFoundPageRules
    feedsRules
