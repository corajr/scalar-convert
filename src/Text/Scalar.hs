module Text.Scalar where

import Data.RDF
import qualified Data.Text as T

import Text.Scalar.RDF

{-| Reads a Scalar RDF/XML string into in-memory RDF. -}
readScalarString :: String -> Either ParseFailure ScalarRDF
readScalarString = parseString (XmlParser Nothing Nothing) . T.pack

{-| Reads a Scalar RDF/XML file into in-memory RDF. -}
readScalarFile :: String -> IO ScalarRDF
readScalarFile = fmap fromEither . parseFile (XmlParser Nothing Nothing)
