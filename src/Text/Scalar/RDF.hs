{-# LANGUAGE OverloadedStrings #-}
module Text.Scalar.RDF ( queryPages
                       , versionFromPageURI
                       , queryContent
                       , queryTitle
                       , findIndex
                       , extractPage
                       , extractAllPages
                       , extractPath
                       , extractAllPaths
                       , ScalarRDF
                       ) where

import Data.RDF
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (find)
import qualified Data.Text as T
import Text.Scalar.Types

import Data.Maybe (listToMaybe)

type ScalarRDF = HashMapS

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
  title' <- queryTitle rdf versionURI
  body <- queryContent rdf versionURI
  return (Page title' body)

-- | Extract all 'Page's in the RDF store.
extractAllPages :: RDF rdf => rdf -> Either ScalarError (Map VersionURI Page)
extractAllPages rdf = fmap (Map.fromList) . mapM getPage $ queryPages rdf
  where getPage pageURI = do
          versionURI <- versionFromPageURI rdf pageURI
          page <- extractPage rdf versionURI
          return (versionURI, page)

-- | Extract a path given the starting 'VersionURI'.
extractPath :: RDF rdf => rdf -> VersionURI -> Either ScalarError Path
extractPath rdf versionURI
  | length pathTargets >= 0 = Right (versionURI : pathTargets)
  | otherwise = Left (ScalarError $ "No path found for " ++ show versionURI)
  where pathResources = map subject $ query rdf Nothing (Just pathHasBody) (Just (UNode (unVersionURI versionURI)))
        getTargets resource = map (mkVersionURI . fromUNode . object) $ query rdf (Just resource) (Just pathHasTarget) Nothing
        pathTargets = concatMap getTargets pathResources

-- | Extract all 'Path's in the RDF store.
extractAllPaths :: RDF rdf => rdf -> Either ScalarError (Map PathID Path)
extractAllPaths = undefined
