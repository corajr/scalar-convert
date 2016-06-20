{-# LANGUAGE TemplateHaskell #-}

module Examples where

import Data.FileEmbed
import qualified Data.ByteString.Char8 as BS

examples :: [(FilePath, BS.ByteString)]
examples = $(embedDir "test/examples")

getExample :: FilePath -> String
getExample ex = case (lookup ex examples) of
  Just s -> BS.unpack s
  Nothing -> error $ ex ++ " not found"
