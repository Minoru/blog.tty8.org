module Debiania.Compilers (
    gzip
  , gzipFileCompiler
) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Hakyll

gzip :: Item String -> Compiler (Item LBS.ByteString)
gzip = withItemBody
         (unixFilterLBS
           "7z"
           [ "a"      -- create archive
           , "dummy"  -- archive's filename (won't be used)
           , "-tgzip" -- archive format
           , "-mx9"   -- m-m-maximum compression
           , "-si"    -- read data from stdin
           , "-so"    -- write archive to stdout
           ]
         . LBS.fromStrict
         . TE.encodeUtf8
         . T.pack)

gzipFileCompiler :: Compiler (Item LBS.ByteString)
gzipFileCompiler = do identifier <- getUnderlying
                      body <- loadBody (setVersion Nothing identifier)
                      makeItem body
                        >>= gzip
