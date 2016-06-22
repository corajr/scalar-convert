{-# LANGUAGE NamedFieldPuns #-}
module Text.Scalar ( module Text.Scalar.Types
                   , readScalarString
                   , readScalarFile
                   , parseScalar
                   , orderPages
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
parseScalar :: RDF rdf => rdf -> ScalarOptions -> Either ScalarError Scalar
parseScalar rdf opts = do
  paths <- extractAllPaths rdf
  pages <- extractAllPages rdf
  return $ Scalar opts paths pages

-- | Collects the 'Page's into a list according to the 'PageOrderStrategy'.
orderPages :: Scalar -> [Page]
orderPages = undefined
