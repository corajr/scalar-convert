{-# LANGUAGE NamedFieldPuns #-}
module Text.Pandoc.Readers.Scalar ( readScalar
                                  , readAndParseScalarFile
                                  , scalarToPandoc
                                  , pageToBlocks
                                  ) where
import Text.Pandoc.Builder
import Text.Pandoc.Error
import Text.Pandoc.Options

import Text.Pandoc.Readers.HTML

import qualified Data.Text as T

import Text.Scalar


-- | Read Scalar RDF/XML from an input string and return a Pandoc document.
readScalar :: ReaderOptions -- ^ Reader options
           -> String        -- ^ String to parse (assuming @'\n'@ line endings)
           -> Either ScalarError Pandoc
readScalar rOpts s = do
  rdf <- readScalarString s
  scalar <- parseScalar rdf def
  scalarToPandoc rOpts scalar

-- | Read Scalar RDF/XML from a file and return a Pandoc document.
readAndParseScalarFile :: FilePath -> ScalarOptions -> IO (Either ScalarError Pandoc)
readAndParseScalarFile path opts = do
  rdf <- readScalarFile path
  let scalar = parseScalar rdf opts
  case scalar of
    Left err -> return (Left err)
    Right scalar' -> return (scalarToPandoc def scalar')

-- | Convert a 'Scalar' to 'Pandoc', or return the error.
scalarToPandoc :: ReaderOptions -> Scalar -> Either ScalarError Pandoc
scalarToPandoc opts scalar =
  go (Right (Pandoc nullMeta [])) (orderPages scalar)
  where go err@(Left _) _ = err
        go doc'@(Right (Pandoc _ _)) [] = doc'
        go (Right (Pandoc meta blocks)) (x:xs) = case pageToBlocks opts x of
          Left err -> Left err
          Right pageBlocks -> go (Right (Pandoc meta (blocks ++ pageBlocks))) xs

-- | Convert a 'Page' to a list of Pandoc 'Block's.
pageToBlocks :: ReaderOptions -> Page -> Either ScalarError [Block]
pageToBlocks opts (Page { pageTitle, pageContent }) = do
  (Pandoc _ blocks) <- liftPandoc $ readHtml opts (T.unpack pageContent)
  return $ (toList (header 1 (text (T.unpack pageTitle)))) <> blocks

liftPandoc :: Either PandocError a -> Either ScalarError a
liftPandoc (Left err) = Left (FromPandoc err)
liftPandoc (Right x) = Right x
