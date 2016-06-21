{-# LANGUAGE OverloadedStrings #-}
module Text.Scalar.RDF ( queryPages
                       , versionFromPageURI
                       , queryContent
                       , queryTitle
                       , findIndex
                       , extractPage
                       , extractPath
                       , extractPagesStartingFrom
                       , extractPagesStartingFromIndex
                       , extractAllPages
                       , ScalarRDF
                       ) where

import Data.RDF
import Data.List (find)
import qualified Data.Text as T
import Text.Scalar.Types

import Control.Monad ((>=>))

import Data.Maybe (listToMaybe)

-- FIXME: Cannot use more efficient HashMapS because Scalar's export of paths depends on order in file.
type ScalarRDF = TriplesList

subject :: Triple -> Subject
subject (Triple x _ _) = x

object :: Triple -> Object
object (Triple _ _ x) = x

fromUNode :: Node -> URI
fromUNode (UNode x) = x
fromUNode err = error $ "Expected UNode but got " ++ show err

fromLNode :: Node -> T.Text
fromLNode (LNode (PlainL x)) = x
fromLNode err = error $ "Expected LNode but got " ++ show err

rdfType :: Node
rdfType = UNode "rdf:type"

version :: Node
version = UNode "scalar:version"

composite :: Node
composite = UNode "http://scalar.usc.edu/2012/01/scalar-ns#Composite"

content :: Node
content = UNode "sioc:content"

title :: Node
title = UNode "dcterms:title"

pathHasBody :: Node
pathHasBody = UNode "oac:hasBody"

pathHasTarget :: Node
pathHasTarget = UNode "oac:hasTarget"

-- | Display an error if an object is not found.
notFound :: String -> Maybe a -> Either ScalarError a
notFound errMsg maybeA =
  case maybeA of
    Just a -> Right a
    Nothing -> Left (ScalarError errMsg)

-- | Find all page URIs in 'RDF'.
queryPages :: RDF rdf => rdf -> [URI]
queryPages rdf = map (fromUNode . subject) $ query rdf Nothing (Just rdfType) (Just composite)

-- | Find the index page URI (of the form /index) or return an error.
findIndex :: RDF rdf => rdf -> Either ScalarError URI
findIndex rdf = notFound err maybePage
  where pages = queryPages rdf
        f x = snd $ T.breakOnEnd "/" x
        maybePage = find ((== "index") . f) pages
        err = "Could not find index in " ++ show pages

-- | Get the corresponding live version for each page 'URI'.
versionFromPageURI :: RDF rdf => rdf -> URI -> Either ScalarError VersionURI
versionFromPageURI rdf pageURI = notFound err . fmap (mkVersionURI . fromUNode . object) . listToMaybe $ query rdf (Just (UNode pageURI)) (Just version) Nothing
  where err = show pageURI ++ " has no corresponding versions."

-- | Extract the text of the object of a predicate from the page version at 'VersionURI'.
queryPageTextObject :: RDF rdf => Node -> rdf -> VersionURI -> Either ScalarError T.Text
queryPageTextObject rdfPred rdf vUri = notFound err . fmap (fromLNode . object) . listToMaybe $ attrs
  where attrs = query rdf (Just (UNode (unVersionURI vUri))) (Just rdfPred) Nothing
        err = show vUri ++ " has no predicate " ++ show rdfPred ++ "."

-- | Extract the content from the page version at 'VersionURI'.
queryContent :: RDF rdf => rdf -> VersionURI -> Either ScalarError T.Text
queryContent = queryPageTextObject content

-- | Extract the title from the page version at 'VersionURI'.
queryTitle :: RDF rdf => rdf -> VersionURI -> Either ScalarError T.Text
queryTitle = queryPageTextObject title

-- | Extract a full 'Page' given the 'VersionURI'.
extractPage :: RDF rdf => rdf -> VersionURI -> Either ScalarError Page
extractPage rdf versionURI = do
  body <- queryContent rdf versionURI
  title' <- queryTitle rdf versionURI
  return (Page versionURI title' body)

-- | Extract a path given the starting 'VersionURI'.
extractPath :: RDF rdf => rdf -> VersionURI -> Either ScalarError Path
extractPath rdf versionURI
  | length pathTargets >= 0 = Right (versionURI : pathTargets)
  | otherwise = Left (ScalarError $ "No path found for " ++ show versionURI)
  where pathResources = map subject $ query rdf Nothing (Just pathHasBody) (Just (UNode (unVersionURI versionURI)))
        getTargets resource = map (mkVersionURI . fromUNode . object) $ query rdf (Just resource) (Just pathHasTarget) Nothing
        pathTargets = concatMap getTargets pathResources

-- | Finds the 'VersionURI' of the given page, then grabs all pages along its path.
extractPagesStartingFrom :: RDF rdf => rdf -> URI -> Either ScalarError [Page]
extractPagesStartingFrom rdf pageURI = do
  versionURI <- versionFromPageURI rdf pageURI
  path <- extractPath rdf versionURI
  mapM (extractPage rdf) path

-- | Attempts to find the index, then grabs all pages along its path.
extractPagesStartingFromIndex :: RDF rdf => rdf -> Either ScalarError [Page]
extractPagesStartingFromIndex rdf = findIndex rdf >>= extractPagesStartingFrom rdf

-- | Extract all 'Page's in the RDF store.
extractAllPages :: RDF rdf => rdf -> Either ScalarError [Page]
extractAllPages rdf = mapM getPage $ queryPages rdf
  where getPage = versionFromPageURI rdf >=> extractPage rdf
