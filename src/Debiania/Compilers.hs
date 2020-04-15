{-# LANGUAGE OverloadedStrings #-}

module Debiania.Compilers (
    gzip
  , gzipFileCompiler
  , debianiaCompiler
) where

import Data.String.Utils (split)
import Text.Pandoc.Options (WriterOptions(writerHTMLMathMethod),
    HTMLMathMethod(MathJax))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Hakyll

-- | Essentially a @pandocCompiler@, but with some preprocessing:
--
-- * @$break$@ on a separate line will be replaced by a section break image.
debianiaCompiler :: Compiler (Item String)
debianiaCompiler =
      getResourceBody
  >>= withItemBody (go . split "\n\n$break$\n\n")
  >>= renderPandocWith
        defaultHakyllReaderOptions
        -- The empty string is path to mathjax.js. We don't want Pandoc to
        -- embed it in output for us as we already do that in Hakyll templates.
        (defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "" })

  where
  go :: [String] -> Compiler String
  go [] = return ""
  go [single] = return single
  go (before:after:rest) = do
    emptyItem <- makeItem ("" :: String)
    newItem <- loadAndApplyTemplate
                 "templates/break.html"
                 (constField "before" before <> constField "after" after)
                 emptyItem
    let newBody = itemBody newItem
    go (newBody:rest)

gzip :: Item String -> Compiler (Item LBS.ByteString)
gzip = withItemBody
         (unixFilterLBS
           "pigz"
           [
           -- Do not store the filename or modification time in the archive.
           --
           -- This works around a problem with site updates. If I re-create
           -- some .html file, its companion .gz will be re-created as well.
           -- However, my rsync invocation compares files by contents, not
           -- modification time. This works great for .html: if the contents
           -- didn't change, rsync won't copy the new file onto the server.
           -- That doesn't work for .gz, though: if it contains .html's
           -- modification time, then .gz has to be copied to the server.
           --
           -- WIth --no-name, .gz should only depend on the contents of .html,
           -- and rsync should skip any .gz that are no different from the
           -- server's.
             "--no-name"
           , "--best"      -- m-m-maximum compression
           , "--to-stdout" -- write archive to stdout
           , "-"           -- read data from stdin
           ]
         . LBS.fromStrict
         . TE.encodeUtf8
         . T.pack)

gzipFileCompiler :: Compiler (Item LBS.ByteString)
gzipFileCompiler = do identifier <- getUnderlying
                      body <- loadBody (setVersion Nothing identifier)
                      makeItem body
                        >>= gzip
