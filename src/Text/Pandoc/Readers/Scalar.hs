{-# LANGUAGE NamedFieldPuns #-}
module Text.Pandoc.Readers.Scalar ( readScalar
                                  , readAndParseScalarFile
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
           -> Either ScalarError Pandoc
readScalar opts s = do
  rdf <- readScalarString s
  scalar <- parseScalar rdf Nothing
  scalarToPandoc opts scalar

-- | Read Scalar RDF/XML from a file and return a Pandoc document.
readAndParseScalarFile :: FilePath -> Maybe URI -> IO (Either ScalarError Pandoc)
readAndParseScalarFile path maybeURI = do
  rdf <- readScalarFile path
  let scalar = parseScalar rdf maybeURI
  case scalar of
    Left err -> return (Left err)
    Right scalar' -> return (scalarToPandoc def scalar')

-- | Convert a 'Scalar' to 'Pandoc', or return the error.
scalarToPandoc :: ReaderOptions -> Scalar -> Either ScalarError Pandoc
scalarToPandoc opts (Scalar { pages }) = go (Right (Pandoc nullMeta [])) pages
  where go err@(Left _) _ = err
        go doc@(Right (Pandoc _ _)) [] = doc
        go (Right (Pandoc meta blocks)) (x:xs) = case pageToBlocks opts x of
          Left err -> Left err
          Right pageBlocks -> go (Right (Pandoc meta (blocks ++ pageBlocks))) xs

-- | Convert a 'Page' to a list of Pandoc 'Block's.
pageToBlocks :: ReaderOptions -> Page -> Either ScalarError [Block]
pageToBlocks opts (Page { pageContent }) = case readHtml opts (T.unpack pageContent) of
  Left err -> Left (FromPandoc err)
  Right (Pandoc _ blocks) -> Right blocks
