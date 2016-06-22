{-# LANGUAGE OverloadedStrings #-}
module Text.Scalar.RDFSpec (main, spec, singlePageVersionURI) where

import Test.Hspec

import Data.RDF
import qualified Data.Text as T

import Examples
import Text.Scalar (readScalarString)
import Text.Scalar.RDF
import Text.Scalar.Types (Page(..), URI, VersionURI, mkVersionURI)

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
      findIndex singlePage `shouldBe` Right indexURI
  describe "versionFromPageURI" $ do
    it "finds the version URI corresponding to a page" $
      versionFromPageURI singlePage indexURI `shouldBe` Right singlePageVersionURI
  describe "queryTitle" $ do
    it "obtains the page title from the VersionURI" $
      queryTitle singlePage singlePageVersionURI `shouldBe` Right "Introduction"
  describe "queryContent" $ do
    it "obtains the page content from the VersionURI" $
      queryContent singlePage singlePageVersionURI `shouldBe` Right singlePageContent
  describe "extractPage" $ do
    it "extracts a page and its contents from the RDF store" $
      extractPage singlePage singlePageVersionURI `shouldBe` Right singlePageScalarPage
  let fullBook = case readScalarString (getExample "full_book.xml") of
        Left err -> error (show err)
        Right x -> x
  describe "extractPath" $ do
    it "gathers all the pages along a path" $
      extractPath fullBook fullBookVersionURI `shouldBe` Right [fullBookVersionURI, page2URI, page3URI]
