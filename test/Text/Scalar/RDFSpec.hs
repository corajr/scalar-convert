{-# LANGUAGE OverloadedStrings #-}
module Text.Scalar.RDFSpec (main, spec, versionURI) where

import Test.Hspec

import Data.RDF
import qualified Data.Text as T

import Examples (getExample, singlePage, singlePageContent)
import Text.Scalar (readScalarString)
import Text.Scalar.RDF
import Text.Scalar.Types (Page(..), URI, VersionURI, mkVersionURI)

main :: IO ()
main = hspec spec

indexURI :: URI
indexURI = "http://scalar.usc.edu/works/scalar-export-test/index"

versionURI :: VersionURI
versionURI = mkVersionURI $ indexURI `mappend` ".1"

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
  describe "versionFromPageURI" $ do
    it "finds the version URI corresponding to a page" $
      versionFromPageURI singlePage indexURI `shouldBe` versionURI
  describe "extractPage" $ do
    it "extracts a page and its contents from the RDF store" $
      extractPage singlePage versionURI `shouldBe` Page versionURI singlePageContent
  let fullBook = fromEither $ readScalarString (getExample "full_book.xml")
  describe "extractPath" $ do
    it "gathers all the pages along a path" $
      extractPath fullBook fullBookVersionURI `shouldBe` [fullBookVersionURI, page2URI, page3URI]
  describe "extractPagesStartingFrom" $ do
    it "gathers pages starting from the specified URI" $
      map pageVersionURI (extractPagesStartingFrom fullBook indexURI) `shouldBe` [fullBookVersionURI, page2URI, page3URI]
