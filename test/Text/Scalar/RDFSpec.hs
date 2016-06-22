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
  describe "findIndex" $ do
    it "returns the URI of the index" $
      findIndex singlePage `shouldBe` Right indexURI
  describe "versionFromPageURI" $ do
    it "finds the version URI corresponding to a page" $
      versionFromPageURI singlePage indexURI `shouldBe` Right versionURI
  describe "queryTitle" $ do
    it "obtains the page title from the versionURI" $
      queryTitle singlePage versionURI `shouldBe` Right "Introduction"
  describe "queryContent" $ do
    it "obtains the page content from the versionURI" $
      queryContent singlePage versionURI `shouldBe` Right singlePageContent
  describe "extractPage" $ do
    it "extracts a page and its contents from the RDF store" $
      extractPage singlePage versionURI `shouldBe` Right (Page "Introduction" singlePageContent)
  let fullBook = case readScalarString (getExample "full_book.xml") of
        Left err -> error (show err)
        Right x -> x
  describe "extractPath" $ do
    it "gathers all the pages along a path" $
      extractPath fullBook fullBookVersionURI `shouldBe` Right [fullBookVersionURI, page2URI, page3URI]
