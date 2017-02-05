{-# LANGUAGE OverloadedStrings #-}

module Debiania.Assets (
    assetsRules
) where

import Hakyll

import Debiania.Compilers

assetsRules :: Rules ()
assetsRules = do
    -- Images and miscellaneous files
    match ( "images/*" .||. "misc/*" .||. "robots.txt" ) $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*.svg" $ version "gzipped" $ do
        route   $ setExtension "svg.gz"
        compile $ do
          getResourceBody
            >>= gzip

    -- MathJax stuff
    match ( "MathJax/MathJax.js" .||.
            "MathJax/extensions/**" .||.
            "MathJax/fonts/**" .||.
            "MathJax/jax/**" .||.
            "MathJax/localization/**") $ do
        route   idRoute
        compile copyFileCompiler

    match "keybase.txt" $ do
        route   idRoute
        compile copyFileCompiler

