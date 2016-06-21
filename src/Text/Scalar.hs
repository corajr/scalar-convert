{-# LANGUAGE NamedFieldPuns #-}
module Text.Scalar ( module Text.Scalar.Types
                   , readScalarString
                   , readScalarFile
                   , parseScalar
                   ) where

import Control.Arrow (left)

import Data.RDF
import qualified Data.Text as T

import Text.Scalar.RDF
import Text.Scalar.Types

-- | Reads a Scalar RDF/XML string into in-memory RDF.
readScalarString :: String -> Either ScalarError ScalarRDF
readScalarString = left RdfError . parseString (XmlParser Nothing Nothing) . T.pack

-- | Reads a Scalar RDF/XML file into in-memory RDF.
readScalarFile :: String -> IO ScalarRDF
readScalarFile = fmap fromEither . parseFile (XmlParser Nothing Nothing)

-- | Parses the RDF into a 'Scalar' (a list of 'Page's and (eventually) some contextual information).
-- Will attempt to extract pages along a path starting from the provided URI.
-- If URI is 'Nothing', it first truies to find a path starting at /index, and then simply
-- grabs all pages.
parseScalar :: RDF rdf => rdf -> ScalarOptions -> Either ScalarError Scalar
parseScalar rdf (ScalarOptions { findPagesBy }) =
  case findPagesBy of
    IndexPath -> Scalar <$> extractPagesStartingFromIndex rdf
    Path pageURI -> Scalar <$> extractPagesStartingFrom rdf pageURI
    GetAll -> Scalar <$> extractAllPages rdf
