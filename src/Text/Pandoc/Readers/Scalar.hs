{-# LANGUAGE OverloadedStrings #-}
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
import Text.Scalar.RDF (versionURItoResourceID, annotationURLtoResourceID)

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
  let content = "<h1>" <> pageTitle <> "</h1>" <> pageContent
  (Pandoc _ blocks) <- liftPandoc $ readHtml opts (T.unpack content)
  return blocks

-- | Returns 'Just' the pandoc content of another page (such as a note or
-- annotation), or 'Nothing'.
pageContentByResourceID :: ReaderOptions -> Scalar -> String -> Maybe [Block]
pageContentByResourceID opts Scalar { scalarPages } resourceID = do
  page <- Map.lookup resourceID pageIndex
  let (eitherBlocks, _) = runScalarM $ pageToBlocks opts page
  case eitherBlocks of
    Left err -> traceShow err Nothing
    Right blocks -> Just blocks
  where pageIndex = Map.mapKeys (fromMaybe "" . versionURItoResourceID) scalarPages

-- | Transform Scalar's notes spans into Pandoc 'Note'
notesTransform :: Scalar -> Inline -> Inline
notesTransform scalar original@(Span ("",["note"],[("rev","scalar:has_note"),("resource",resourceID)]) inlines) =
  maybe original f otherPage
  where otherPage = pageContentByResourceID def scalar resourceID
        f (_:blocks) = Span nullAttr (inlines ++ [Note blocks])
        f [] = original
notesTransform _ x = x

-- | Changes annotations to footnotes containing the body of the annotation.
annotationTransform :: Scalar -> Inline -> Inline
annotationTransform scalar original@(Link ("",classes,[]) inlines (annotationURL,""))
  | '#' `elem` annotationURL =  fromMaybe original $ do
      blocks <- maybeBlocks
      let noteContents = Plain [preFragLink] : blocks
      return $ Span nullAttr (inlines' ++ [Note noteContents])
  | otherwise = original
  where preFragmentURL = takeWhile (/= '#') annotationURL
        preFragLink = Link ("",[],[]) [Str preFragmentURL] (preFragmentURL, "")
        inlines' = if classes == ["inline"]
                   then [inlineMediaLink preFragmentURL]
                   else inlines
        maybeResource = annotationURLtoResourceID annotationURL
        maybeBlocks = maybeResource >>= pageContentByResourceID def scalar
annotationTransform _ x = x

-- | Makes an inline media link from the media URL.
inlineMediaLink :: String -> Inline
inlineMediaLink mediaURL = head . toList $
  spanWith nullAttr (text "[Media: " <> link mediaURL "" (text mediaURL) <> str "]")

-- | Adds the URL to the text of inline media links so that the link isn't blank.
inlineMediaTransform :: Scalar -> Inline -> Inline
inlineMediaTransform _ (Link ("",["inline"],[]) [] (mediaURL,"")) =
  inlineMediaLink mediaURL
inlineMediaTransform _ x = x

-- | Applies all selected transforms at the inline and block level.
applyTransforms :: Scalar -> Pandoc -> Pandoc
applyTransforms = applyInlineTransforms

-- | Select which transformations should apply to this 'Scalar'.
selectInlineTransforms :: Scalar -> [InlineTransform]
selectInlineTransforms _ =
  [ notesTransform
  , annotationTransform
  , inlineMediaTransform
  ]

-- | Applies inline transformations (such as notes).
applyInlineTransforms :: Scalar -> Pandoc -> Pandoc
applyInlineTransforms scalar start = foldl' f start transforms
  where f acc x = walk (x scalar) acc
        transforms = selectInlineTransforms scalar

liftPandoc :: Either PandocError a -> ScalarM a
liftPandoc (Left err) = throwError (FromPandoc err)
liftPandoc (Right x) = return x
