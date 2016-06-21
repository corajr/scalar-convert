{-# LANGUAGE NamedFieldPuns #-}

module Text.Pandoc.Readers.Scalar ( readScalar
                                  , scalarToPandoc
                                  , pageToBlocks
                                  ) where

import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Options

import Text.Pandoc.Readers.HTML

import qualified Data.Text as T

import Text.Scalar

-- | Read Scalar RDF/XML from an input string and return a Pandoc document.
readScalar :: ReaderOptions -- ^ Reader options
           -> String        -- ^ String to parse (assuming @'\n'@ line endings)
           -> Either PandocError Pandoc
readScalar opts s = do
  rdf <- case readScalarString s of
    Left err -> Left (ParseFailure (show err))
    Right graph -> Right graph
  let scalar = parseScalar rdf
  scalarToPandoc opts scalar

scalarToPandoc :: ReaderOptions -> Scalar -> Either PandocError Pandoc
scalarToPandoc opts (Scalar { pages }) = go (Right (Pandoc nullMeta [])) pages
  where go err@(Left _) _ = err
        go doc@(Right (Pandoc _ _)) [] = doc
        go (Right (Pandoc meta blocks)) (x:xs) = case pageToBlocks opts x of
          Left err -> Left err
          Right pageBlocks -> go (Right (Pandoc meta (blocks ++ pageBlocks))) xs

pageToBlocks :: ReaderOptions -> Page -> Either PandocError [Block]
pageToBlocks opts (Page { pageContent }) = case readHtml opts (T.unpack pageContent) of
  Left err -> Left err
  Right (Pandoc _ blocks) -> Right blocks
