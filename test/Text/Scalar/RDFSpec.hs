{-# LANGUAGE OverloadedStrings #-}
module Text.Scalar.RDFSpec (main, spec, versionURI) where

import Test.Hspec

import Data.RDF
import qualified Data.Text as T

import Examples (singlePage, singlePageContent)
import Text.Scalar.RDF
import Text.Scalar.Types (Page(..), URI, VersionURI, mkVersionURI)

main :: IO ()
main = hspec spec

indexURI :: URI
indexURI = "http://scalar.usc.edu/works/scalar-export-test/index"

versionURI :: VersionURI
versionURI = mkVersionURI $ indexURI `mappend` ".1"

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
