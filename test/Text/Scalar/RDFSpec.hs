{-# LANGUAGE OverloadedStrings #-}
module Text.Scalar.RDFSpec (main, spec, singlePageVersionURI) where

import Test.Hspec

import Data.RDF
import qualified Data.Text as T
import qualified Data.Map as Map

import Examples
import Text.Scalar (readScalarString)
import Text.Scalar.RDF
import Text.Scalar.Types

main :: IO ()
main = hspec spec

fullBookVersionURI :: VersionURI
fullBookVersionURI = mkVersionURI "http://scalar.usc.edu/works/scalar-export-test/index.4"

page2URI :: VersionURI
page2URI = mkVersionURI "http://scalar.usc.edu/works/scalar-export-test/following-a-path.1#index=1"

page3URI :: VersionURI
page3URI = mkVersionURI "http://scalar.usc.edu/works/scalar-export-test/notes-and-media.2#index=2"

spec :: Spec
spec = do
  describe "queryPages" $ do
    it "finds all page URIs from an RDF graph" $
      queryPages singlePage `shouldBe` [indexURI]
  describe "findIndex" $ do
    it "returns the URI of the index" $
      findIndex singlePage `shouldBeScalar` Right indexURI
  describe "versionFromPageURI" $ do
    it "finds the version URI corresponding to a page" $
      versionFromPageURI singlePage indexURI `shouldBeScalar` Right singlePageVersionURI
  describe "versionURItoResourceID" $ do
    it "returns the resource ID for the URI" $
      versionURItoResourceID singlePageVersionURI `shouldBe` Just "index"
  describe "queryTitle" $ do
    it "obtains the page title from the VersionURI" $
      queryTitle singlePage singlePageVersionURI `shouldBeScalar` Right "Introduction"
  describe "queryContent" $ do
    it "obtains the page content from the VersionURI" $
      queryContent singlePage singlePageVersionURI `shouldBeScalar` Right singlePageContent
  describe "extractPage" $ do
    it "extracts a page and its contents from the RDF store" $
      extractPage singlePage singlePageVersionURI `shouldBeScalar` Right singlePageScalarPage
  let fullBookIndexPath = [fullBookVersionURI, page2URI, page3URI]
  describe "extractAllPages" $ do
    it "extracts all pages from the RDF store" $
      extractAllPages fullBookRdf `shouldSatisfyScalar` (\(Right m) -> Map.size m == 5)
  describe "parsePathTarget" $ do
    it "converts a URI on a 'Path' to a 'PathComponent'" $ do
      let pathURI = "http://scalar.usc.edu/works/scalar-export-test/following-a-path.1#index=1"
          pathComponent = PathComponent { pathIndex = 1
                                        , pathVersionURI = page2URI }
      parsePathTarget pathURI `shouldBe` Just pathComponent
    it "returns nothing for an invalid path target" $ do
      let invalidPathURI = "http://scalar.usc.edu/works/scalar-export-test/following-a-path.1"
      parsePathTarget invalidPathURI `shouldBe` Nothing
  describe "extractAllPaths" $ do
    it "extracts all paths from the RDF store" $
      extractAllPaths fullBookRdf `shouldBeScalar` Right (Map.singleton (mkPathID "index") fullBookIndexPath)
