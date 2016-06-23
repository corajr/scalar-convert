{-# LANGUAGE NamedFieldPuns #-}
module Text.Pandoc.Readers.Scalar ( readScalar
                                  , readAndParseScalarFile
                                  , scalarToPandoc
                                  , pageToBlocks
                                  , applyTransforms
                                  , selectInlineTransforms
                                  , applyInlineTransforms
                                  , notesTransform
                                  ) where
import Text.Pandoc.Builder
import Text.Pandoc.Error
import Text.Pandoc.Walk
import Text.Pandoc.Options

import Text.Pandoc.Readers.HTML
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)
import qualified Data.Map as Map

import qualified Data.Text as T

import Text.Scalar
import Text.Scalar.RDF (versionURItoResourceID)

import Control.Monad.Except

type InlineTransform = Scalar -> Inline -> Inline

-- | Read Scalar RDF/XML from an input string and return a Pandoc document.
readScalar :: ReaderOptions -- ^ Pandoc HTML reader options
           -> ScalarOptions -- ^ Scalar options
           -> String        -- ^ String to parse (assuming @'\n'@ line endings)
           -> ScalarM Pandoc
readScalar rOpts sOpts s = do
  rdf <- readScalarString s
  scalar <- parseScalar rdf sOpts
  scalarToPandoc rOpts scalar

-- | Read Scalar RDF/XML from a file and return a Pandoc document.
readAndParseScalarFile :: FilePath -> ScalarOptions -> IO (ScalarM Pandoc)
readAndParseScalarFile path opts = do
  rdf <- readScalarFile path
  return $ parseScalar rdf opts >>= scalarToPandoc def

-- | Convert a 'Scalar' to 'Pandoc', or return the error.
scalarToPandoc :: ReaderOptions -> Scalar -> ScalarM Pandoc
scalarToPandoc opts scalar = do
  pages <- orderPages scalar
  blocks <- mapM (pageToBlocks opts) pages
  let doc' = Pandoc nullMeta (concat blocks)
  return $ applyTransforms scalar doc'

-- | Convert a 'Page' to a list of Pandoc 'Block's.
pageToBlocks :: ReaderOptions -> Page -> ScalarM [Block]
pageToBlocks opts Page { pageTitle, pageContent } = do
  (Pandoc _ blocks) <- liftPandoc $ readHtml opts (T.unpack pageContent)
  return $ toList (header 1 (text (T.unpack pageTitle))) <> blocks

-- | Transform Scalar's notes spans into Pandoc 'Note'
notesTransform :: Scalar -> Inline -> Inline
notesTransform Scalar { scalarPages } original@(Span ("",["note"],[("rev","scalar:has_note"),("resource",resourceID)]) inlines) =
  fromMaybe original otherPage
  where pageIndex = Map.mapKeys (fromMaybe "" . versionURItoResourceID) scalarPages
        otherPage = do
          page <- Map.lookup resourceID pageIndex
          let (eitherBlocks, _) = runScalarM $ pageToBlocks def page
          case eitherBlocks of
            Left err -> traceShow err Nothing
            Right (_:blocks) -> return $ Span nullAttr (inlines ++ [Note blocks])
            Right [] -> Nothing
notesTransform _ x = x

-- | Applies all selected transforms at the inline and block level.
applyTransforms :: Scalar -> Pandoc -> Pandoc
applyTransforms = applyInlineTransforms

-- | Select which transformations should apply to this 'Scalar'.
selectInlineTransforms :: Scalar -> [InlineTransform]
selectInlineTransforms _ =
  [ notesTransform
  ]

-- | Applies inline transformations (such as notes).
applyInlineTransforms :: Scalar -> Pandoc -> Pandoc
applyInlineTransforms scalar start = foldl' f start transforms
  where f acc x = walk (x scalar) acc
        transforms = selectInlineTransforms scalar

liftPandoc :: Either PandocError a -> ScalarM a
liftPandoc (Left err) = throwError (FromPandoc err)
liftPandoc (Right x) = return x
