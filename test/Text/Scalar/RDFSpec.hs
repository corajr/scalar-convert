{-# LANGUAGE OverloadedStrings #-}
module Text.Scalar.RDFSpec where

import Test.Hspec

import Data.RDF
import qualified Data.Text as T

import Examples (singlePage)
import Text.Scalar.RDF

main :: IO ()
main = hspec spec

indexURI :: T.Text
indexURI = "http://scalar.usc.edu/works/scalar-export-test/index"

spec :: Spec
spec = do
  describe "queryPages" $ do
    it "finds all page URIs from an RDF graph" $
      queryPages singlePage `shouldBe` [indexURI]
  describe "versionFromPageURI" $ do
    it "finds the version URI corresponding to a page" $
      versionFromPageURI singlePage indexURI `shouldBe` (indexURI `mappend` ".1")
