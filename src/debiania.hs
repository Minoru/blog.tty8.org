{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))

import Hakyll

import Debiania.Archives
import Debiania.Assets
import Debiania.CSS
import Debiania.Compilers
import Debiania.Feeds
import Debiania.Posts
import Debiania.Settings

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

    create ["index.html"] $ do
        route     idRoute
        compile $ do
          posts <- fmap (take 8) . recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
          let ctx =    constField "title" "Home"
                    <> constField "navbar-home" "Yep"
                    <> listField "posts" postCtx (return posts)
                    <> debianiaCtx

          makeItem ""
            >>= loadAndApplyTemplate "templates/index.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    postsRules
    archivesRules

    -- Render About, Subscribe and 404 pages
    create ["about.markdown"] $ do
        route   $ setExtension "html"
        compile $ debianiaCompiler
          >>= loadAndApplyTemplate "templates/about.html" debianiaCtx
          >>= loadAndApplyTemplate
                "templates/default.html"
                (debianiaCtx <> constField "navbar-about" "Yep")
          >>= relativizeUrls

    create ["subscribe.markdown"] $ do
        route   $ setExtension "html"
        compile $ debianiaCompiler
          >>= loadAndApplyTemplate "templates/about.html" debianiaCtx
          >>= loadAndApplyTemplate
                "templates/default.html"
                (debianiaCtx <> constField "navbar-subscribe" "Yep")
          >>= relativizeUrls

    create ["404.markdown"] $ do
        route   $ setExtension "html"
        compile $ debianiaCompiler
          >>= loadAndApplyTemplate "templates/about.html" debianiaCtx
          >>= loadAndApplyTemplate "templates/default.html" debianiaCtx
          >>= relativizeUrls

    create ["sitemap.xml"] $ do
       route   idRoute
       compile $ do
         posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
         let sitemapCtx =    listField "posts" postCtx (return posts)
                          <> debianiaCtx
         makeItem ""
          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    create ["sitemap.xml"] $ version "gzipped" $ do
        route   $ setExtension "xml.gz"
        compile gzipFileCompiler

    create ["about.markdown"
           , "subscribe.markdown"
           , "404.markdown"
           , "index.html"]
      $ version "gzipped" $ do
        route   $ setExtension "html.gz"
        compile gzipFileCompiler

    feedsRules
