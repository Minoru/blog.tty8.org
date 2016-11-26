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
import Debiania.Settings
import Debiania.Sitemap
import Debiania.SubscribePage

main :: IO ()
main = hakyllWith debianiaConfig $ do
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
