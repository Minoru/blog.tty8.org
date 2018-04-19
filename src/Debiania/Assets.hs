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
            "MathJax/fonts/HTML-CSS/Asana-Math/**" .||.
            "MathJax/fonts/HTML-CSS/Gyre-Pagella/**" .||.
            "MathJax/fonts/HTML-CSS/Gyre-Termes/**" .||.
            "MathJax/fonts/HTML-CSS/Latin-Modern/**" .||.
            "MathJax/fonts/HTML-CSS/Neo-Euler/**" .||.
            "MathJax/fonts/HTML-CSS/STIX-Web/**" .||.
            "MathJax/fonts/HTML-CSS/TeX/eot/**" .||.
            "MathJax/fonts/HTML-CSS/TeX/otf/**" .||.
            "MathJax/fonts/HTML-CSS/TeX/svg/**" .||.
            "MathJax/fonts/HTML-CSS/TeX/woff/**" .||.
            "MathJax/jax/**" .||.
            "MathJax/localization/**") $ do
        route   idRoute
        compile copyFileCompiler

    match "keybase.txt" $ do
        route   idRoute
        compile copyFileCompiler

