{-# LANGUAGE OverloadedStrings #-}

module Debiania.CSS (
    cssRules
) where

import Hakyll

import Debiania.Compilers

cssRules :: Rules ()
cssRules = do
    match "css/*" $ do
        -- No `route` rule means these files won't be present in generated
        -- site. The point of this whole block is to compress the files and put
        -- them into the cache so that later on, we can use `loadBody` to take
        -- them out and generate debiania.css out of them.
        compile compressCssCompiler

    create ["css/debiania.css"] $ do
        route idRoute
        compile $ do
          styles <- mapM
                      (loadBody . fromFilePath . ("css/"++))
                      [ "normalize.css"
                      , "syntax.css"
                      , "default.css"
                      , "desktop.css"
                      , "mobile.css"]
          makeItem (concat (styles :: [String]))

    create ["css/debiania.css.gz"] $ do
        route idRoute
        compile $ do
          css <- load "css/debiania.css"
          makeItem (itemBody css)
            >>= gzip

