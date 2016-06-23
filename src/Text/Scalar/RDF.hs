{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Scalar.RDF ( queryPages
                       , versionFromPageURI
                       , versionURItoResourceID
                       , bodyURItoPathID
                       , queryContent
                       , queryTitle
                       , findIndex
                       , extractPage
                       , extractAllPages
                       , parsePathTarget
                       , extractAllPaths
                       , ScalarRDF
                       ) where

import Data.RDF
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (find, sort)
import qualified Data.Text as T

import Text.Regex.PCRE
import Text.Scalar.Types

import Data.Maybe (listToMaybe, catMaybes)

import Control.Arrow ((&&&))

import Control.Monad.Except
import Control.Monad.Writer

type ScalarRDF = HashMapS

subject :: Triple -> Subject
subject (Triple x _ _) = x

object :: Triple -> Object
object (Triple _ _ x) = x

subjectAndObject :: Triple -> (URI, URI)
subjectAndObject = (textFromNode . subject) &&& (textFromNode . object)

textFromNode :: Node -> T.Text
textFromNode (UNode x) = x
textFromNode (LNode (PlainL x)) = x
textFromNode err = error $ "Don't know how to parse node " ++ show err

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
notFound :: String -> Maybe a -> ScalarM a
notFound errMsg maybeA =
  case maybeA of
    Just a -> return a
    Nothing -> tell errMsg >> throwError (ScalarError errMsg)

-- | Find all page URIs in 'RDF'.
queryPages :: RDF rdf => rdf -> [URI]
queryPages rdf = map (fromUNode . subject) $ query rdf Nothing (Just rdfType) (Just composite)

-- | Find the index page URI (of the form /index) or return an error.
findIndex :: RDF rdf => rdf -> ScalarM URI
findIndex rdf = notFound err maybePage
  where pages = queryPages rdf
        f x = snd $ T.breakOnEnd "/" x
        maybePage = find ((== "index") . f) pages
        err = "Could not find index in " ++ show pages

-- | Get the corresponding live version for each page 'URI'.
versionFromPageURI :: RDF rdf => rdf -> URI -> ScalarM VersionURI
versionFromPageURI rdf pageURI = notFound err . fmap (mkVersionURI . fromUNode . object) . listToMaybe $ query rdf (Just (UNode pageURI)) (Just version) Nothing
  where err = show pageURI ++ " has no corresponding versions."

-- | Turn a 'VersionURI' into an identifier, e.g. "version.1" -> "version"
versionURItoResourceID :: VersionURI -> Maybe String
versionURItoResourceID vUri = do
  let uri = T.unpack (unVersionURI vUri)
      regResult :: Maybe (AllTextSubmatches [] String) = uri =~~ ("/([^./]+?)\\.\\d+$" :: String)
  reg <- regResult
  let matches = getAllTextSubmatches reg
  guard $ length matches == 2
  let [_, resourceID] = matches
  return resourceID

-- | Extract the text of the object of a predicate from the page version at 'VersionURI'.
queryPageTextObject :: RDF rdf => Node -> rdf -> VersionURI -> ScalarM T.Text
queryPageTextObject rdfPred rdf vUri = notFound err . fmap (fromLNode . object) . listToMaybe $ attrs
  where attrs = query rdf (Just (UNode (unVersionURI vUri))) (Just rdfPred) Nothing
        err = show vUri ++ " has no predicate " ++ show rdfPred ++ ".\n"

-- | Extract the content from the page version at 'VersionURI'.
queryContent :: RDF rdf => rdf -> VersionURI -> ScalarM T.Text
queryContent = queryPageTextObject content

-- | Extract the title from the page version at 'VersionURI'.
queryTitle :: RDF rdf => rdf -> VersionURI -> ScalarM T.Text
queryTitle = queryPageTextObject title

-- | Extract a full 'Page' given the 'VersionURI'.
extractPage :: RDF rdf => rdf -> VersionURI -> ScalarM Page
extractPage rdf versionURI = do
  title' <- queryTitle rdf versionURI
  body <- queryContent rdf versionURI
  return (Page title' body)

-- | Extract all 'Page's in the RDF store.
extractAllPages :: RDF rdf => rdf -> ScalarM (Map VersionURI Page)
extractAllPages rdf = fmap (Map.fromList . catMaybes) . mapM getPage $ queryPages rdf
  where getPage pageURI = do
          versionURI <- versionFromPageURI rdf pageURI
          do page <- extractPage rdf versionURI
             return $ Just (versionURI, page)
          `catchError` (return . const Nothing)

-- | Takes a path target of the form "[versionUri]#index=[index]" and converts it to a
-- 'PathComponent'
parsePathTarget :: URI -> Maybe PathComponent
parsePathTarget uri = do
  let uri' = T.unpack uri
      regResult :: Maybe (AllTextSubmatches [] String) = uri' =~~ ("([^#]+)#index=(\\d+)" :: String)
  reg <- regResult
  let matches = getAllTextSubmatches reg
  guard $ length matches == 3
  let [_, versionURI, index] = matches
  return PathComponent { pathIndex = read index
                       , pathVersionURI = mkVersionURI (T.pack versionURI)
                       }

bodyURItoPathID :: PathBodyURI -> PathID
bodyURItoPathID uri = (mkPathID . maybe "" T.pack) (versionURItoResourceID vUri)
  where vUri = mkVersionURI uri

-- | Extract all 'Path's in the RDF store.
extractAllPaths :: RDF rdf => rdf -> ScalarM (Map PathID Path)
extractAllPaths rdf = do
  let resourceToBody :: Map PathResourceURI PathBodyURI = Map.fromList . map subjectAndObject $ query rdf Nothing (Just pathHasBody) Nothing
      resourceToTarget :: Map PathResourceURI PathTargetURI = Map.fromList . map subjectAndObject $ query rdf Nothing (Just pathHasTarget) Nothing
      resourceToPathComponent :: Map PathResourceURI PathComponent = Map.mapMaybe parsePathTarget resourceToTarget
      bodyAndPathComponents :: [(PathBodyURI, PathComponent)] = Map.elems $ Map.intersectionWith (,) resourceToBody resourceToPathComponent
      pathIDtoBodyAndPathComponentList :: Map PathID [(PathBodyURI, PathComponent)] = Map.fromListWith (++) $ map ((bodyURItoPathID . fst) &&& (:[])) bodyAndPathComponents
      pathIDtoPath :: Map PathID Path = Map.map (\xs@(x:_) -> mkVersionURI (fst x) : map pathVersionURI (sort (map snd xs))) pathIDtoBodyAndPathComponentList
  return pathIDtoPath
