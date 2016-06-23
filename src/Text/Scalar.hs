{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Scalar ( module Text.Scalar.Types
                   , readScalarString
                   , readScalarFile
                   , parseScalar
                   , orderPages
                   ) where

import Control.Arrow (left)

import Data.Maybe (fromMaybe, mapMaybe)
import Data.RDF
import qualified Data.Map as Map
import qualified Data.Text as T

import Text.Scalar.RDF
import Text.Scalar.Types

-- | Reads a Scalar RDF/XML string into in-memory RDF.
readScalarString :: String -> ScalarM ScalarRDF
readScalarString = left RdfError . parseString (XmlParser Nothing Nothing) . T.pack

-- | Reads a Scalar RDF/XML file into in-memory RDF.
readScalarFile :: String -> IO ScalarRDF
readScalarFile = fmap fromEither . parseFile (XmlParser Nothing Nothing)

-- | Parses the RDF into a 'Scalar' (a list of 'Page's and (eventually) some contextual information).
parseScalar :: RDF rdf => rdf -> ScalarOptions -> ScalarM Scalar
parseScalar rdf opts = do
  paths <- extractAllPaths rdf
  pages <- extractAllPages rdf
  return $ Scalar opts paths pages

-- | Collects the 'Page's into a list according to the 'PageOrderStrategy',
-- or returns an error
orderPages :: Scalar -> ScalarM [Page]
orderPages scalar@(Scalar { scalarOptions, scalarPages }) =
  case orderPagesBy scalarOptions of
    IndexPath -> getPath scalar (mkPathID "index")
    Path s -> getPath scalar (mkPathID s)
    None -> return $ Map.elems scalarPages

-- | Attempts to get the specified 'PathID' or returns an error.
getPath :: Scalar -> PathID -> ScalarM [Page]
getPath (Scalar { scalarPaths, scalarPages }) path =
  maybe (Left (ScalarError err)) Right pathResult
  where pathResult = do
          path' <- Map.lookup path scalarPaths
          return $ mapMaybe ((flip Map.lookup) scalarPages) path'
        err = "Could not find path " ++ show (unPathID path) ++ ". Available paths are:\n" ++
              concatMap ((\xs -> "- " ++ xs ++ "\n") . T.unpack . unPathID) (Map.keys scalarPaths)
