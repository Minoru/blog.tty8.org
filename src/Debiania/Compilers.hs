{-# LANGUAGE OverloadedStrings #-}

module Debiania.Compilers (
    gzip
  , gzipFileCompiler
  , debianiaCompiler
) where

import Text.Pandoc (Pandoc, Block(Header), Inline(Link))
import Text.Pandoc.Options (WriterOptions(writerHTMLMathMethod),
    HTMLMathMethod(MathJax))
import Text.Pandoc.Walk (walk)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Hakyll

-- | Renders item with Pandoc, using custom MathJax path
debianiaCompiler :: Compiler (Item String)
debianiaCompiler =
      getResourceBody
  >>= renderPandocWithTransform
        defaultHakyllReaderOptions
        -- The empty string is path to mathjax.js. We don't want Pandoc to
        -- embed it in output for us as we already do that in Hakyll templates.
        (defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "" })
        addHeaderLinks

-- | Turns headings into links to themselves, so readers can copy-paste a link
-- to the particular section.
--
-- This code is adapted from https://stackoverflow.com/a/77944577/2350060
addHeaderLinks :: Pandoc -> Pandoc
addHeaderLinks = walk transformHeader
  where
  transformHeader :: Block -> Block
  transformHeader (Header level attrs@(identifier, _, _) contents) =
    let
      linkClass = "headerLink"
      linkAttrs = ("", [linkClass], [])
      linkDestination = "#" <> identifier

      anchorClass = "anchor"
      anchorAttrs = ("", [anchorClass], [])
    in Header level attrs
        [ Link anchorAttrs [] (linkDestination, "")
        , Link linkAttrs contents (linkDestination, "")
        ]
  transformHeader block = block

-- | Compresses given item with pigz (parallel gzip compiler)
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

-- | Compresse curent item with pigz (parallel gzip compiler)
gzipFileCompiler :: Compiler (Item LBS.ByteString)
gzipFileCompiler = do identifier <- getUnderlying
                      body <- loadBody (setVersion Nothing identifier)
                      makeItem body
                        >>= gzip
