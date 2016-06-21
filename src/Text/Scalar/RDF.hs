{-# LANGUAGE OverloadedStrings #-}
module Text.Scalar.RDF ( queryPages
                       , versionFromPageURI
                       , queryContent
                       , extractPage
                       , extractPath
                       , extractPagesOnPath
                       , extractAllPages
                       , ScalarRDF
                       ) where

import Data.RDF
import qualified Data.Text as T
import Text.Scalar.Types

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

pathHasBody :: Node
pathHasBody = UNode "oac:hasBody"

pathHasTarget :: Node
pathHasTarget = UNode "oac:hasTarget"

-- | Find all page URIs in 'RDF'.
queryPages :: RDF rdf => rdf -> [URI]
queryPages rdf = map (fromUNode . subject) $ query rdf Nothing (Just rdfType) (Just composite)

-- | Get the corresponding live version for each page 'URI'.
versionFromPageURI :: RDF rdf => rdf -> URI -> VersionURI
versionFromPageURI rdf pageURI = mkVersionURI . fromUNode . object . head $ query rdf (Just (UNode pageURI)) (Just version) Nothing

-- | Extract the content from a version.
queryContent :: RDF rdf => rdf -> VersionURI -> T.Text
queryContent rdf vUri = fromLNode . object . head $ query rdf (Just (UNode (unVersionURI vUri))) (Just content) Nothing

-- | Extract a full 'Page' given the 'VersionURI'.
extractPage :: RDF rdf => rdf -> VersionURI -> Page
extractPage rdf versionURI = Page versionURI body
  where body = queryContent rdf versionURI

-- | Extract a path given the starting 'VersionURI'.
extractPath :: RDF rdf => rdf -> VersionURI -> Path
extractPath rdf versionURI = versionURI : pathTargets
  where pathResources = map subject $ query rdf Nothing (Just pathHasBody) (Just (UNode (unVersionURI versionURI)))
        getTargets resource = map (mkVersionURI . fromUNode . object) $ query rdf (Just resource) (Just pathHasTarget) Nothing
        pathTargets = concatMap getTargets pathResources

-- | Extract the 'Page's along a 'Path'.
extractPagesOnPath :: RDF rdf => rdf -> Path -> [Page]
extractPagesOnPath rdf = map (extractPage rdf)

-- | Extract all 'Page's in the RDF store.
extractAllPages :: RDF rdf => rdf -> [Page]
extractAllPages rdf = map (extractPage rdf . versionFromPageURI rdf) $ queryPages rdf
