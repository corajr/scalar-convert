{-# LANGUAGE OverloadedStrings #-}
module Text.Scalar.RDF ( queryPages
                       , versionFromPageURI
                       , queryContent
                       , extractPage
                       , extractAllPages
                       , ScalarRDF
                       ) where

import Data.RDF
import qualified Data.Text as T
import Text.Scalar.Types

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

-- | Find all page URIs in 'RDF'.
queryPages :: RDF rdf => rdf -> [URI]
queryPages rdf = map (fromUNode . subject) $ query rdf Nothing (Just rdfType) (Just composite)

-- | Get the corresponding live version for each page 'URI'.
versionFromPageURI :: RDF rdf => rdf -> URI -> VersionURI
versionFromPageURI rdf pageURI = mkVersionURI . fromUNode . object . head $ query rdf (Just (UNode pageURI)) (Just version) Nothing

-- | Extract the content from a version.
queryContent :: RDF rdf => rdf -> VersionURI -> T.Text
queryContent rdf vUri = fromLNode . object . head $ query rdf (Just (UNode (unVersionURI vUri))) (Just content) Nothing

-- | Extract a full 'Page' given the version 'URI'.
extractPage :: RDF rdf => rdf -> VersionURI -> Page
extractPage rdf versionURI = Page versionURI body
  where body = queryContent rdf versionURI

-- | Extract all 'Page's in the RDF store.
extractAllPages :: RDF rdf => rdf -> [Page]
extractAllPages rdf = map (extractPage rdf . versionFromPageURI rdf) $ queryPages rdf
