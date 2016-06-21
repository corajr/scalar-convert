{-# LANGUAGE OverloadedStrings #-}
module Text.Scalar.RDF where

import Data.RDF
import qualified Data.Text as T

type ScalarRDF = HashMapS
type URI = T.Text

subject :: Triple -> Subject
subject (Triple x _ _) = x

predicate :: Triple -> Predicate
predicate (Triple _ x _) = x

object :: Triple -> Object
object (Triple _ _ x) = x

fromUNode :: Node -> URI
fromUNode (UNode x) = x
fromUNode err = error $ "Expected UNode but got " ++ show err

rdfType :: Node
rdfType = UNode "rdf:type"

version :: Node
version = UNode "scalar:version"

composite :: Node
composite = UNode "http://scalar.usc.edu/2012/01/scalar-ns#Composite"

queryPages :: RDF rdf => rdf -> [URI]
queryPages rdf = map (fromUNode . subject) $ query rdf Nothing (Just rdfType) (Just composite)

versionFromPageURI :: RDF rdf => rdf -> URI -> URI
versionFromPageURI rdf pageURI = fromUNode . object . head $ query rdf (Just (UNode pageURI)) (Just version) Nothing
