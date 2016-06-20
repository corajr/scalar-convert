module Text.Scalar where

import Data.RDF
import qualified Data.Text as T

{-| Reads a Scalar RDF/XML string into in-memory RDF. -}
readScalarString :: String -> Either ParseFailure HashMapSP
readScalarString = parseString (XmlParser Nothing Nothing) . T.pack

readScalarFile :: String -> IO HashMapSP
readScalarFile = fmap fromEither . parseFile (XmlParser Nothing Nothing)
