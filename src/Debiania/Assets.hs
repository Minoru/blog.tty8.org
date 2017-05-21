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
            "MathJax/fonts/Asana-Math/**" .||.
            "MathJax/fonts/Gyre-Pagella/**" .||.
            "MathJax/fonts/Gyre-Termes/**" .||.
            "MathJax/fonts/Latin-Modern/**" .||.
            "MathJax/fonts/Neo-Euler/**" .||.
            "MathJax/fonts/STIX-Web/**" .||.
            "MathJax/fonts/TeX/eot/**" .||.
            "MathJax/fonts/TeX/otf/**" .||.
            "MathJax/fonts/TeX/svg/**" .||.
            "MathJax/fonts/TeX/woff/**" .||.
            "MathJax/jax/**" .||.
            "MathJax/localization/**") $ do
        route   idRoute
        compile copyFileCompiler

    match "keybase.txt" $ do
        route   idRoute
        compile copyFileCompiler

