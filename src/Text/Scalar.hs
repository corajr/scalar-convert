module Text.Scalar ( module Text.Scalar.Types
                   , readScalarString
                   , readScalarFile
                   , parseScalar
                   ) where

import Data.RDF
import qualified Data.Text as T

import Text.Scalar.RDF
import Text.Scalar.Types

-- | Reads a Scalar RDF/XML string into in-memory RDF.
readScalarString :: String -> Either ParseFailure ScalarRDF
readScalarString = parseString (XmlParser Nothing Nothing) . T.pack

-- | Reads a Scalar RDF/XML file into in-memory RDF.
readScalarFile :: String -> IO ScalarRDF
readScalarFile = fmap fromEither . parseFile (XmlParser Nothing Nothing)

-- | Parses the RDF into a 'Scalar' (a list of 'Page's and (eventually) some contextual information).
-- Will attempt to extract pages along a path starting from the provided URI, or all pages if no path is given.
parseScalar :: RDF rdf => rdf -> Maybe URI -> Scalar
parseScalar rdf maybeURI =
  case maybeURI of
    Just pageURI -> Scalar $ extractPagesStartingFrom rdf pageURI
    Nothing -> Scalar $ extractAllPages rdf
