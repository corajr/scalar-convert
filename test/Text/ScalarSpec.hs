{-# LANGUAGE OverloadedStrings #-}
module Text.ScalarSpec (main, spec, singlePageScalar) where

import Test.Hspec
import Test.QuickCheck

import Data.Default (def)
import Data.List (isPrefixOf)
import Data.RDF

import qualified Data.Map as Map

import Examples

import Text.Scalar

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "readScalarString" $ do
    it "reads a Scalar RDF/XML string into in-memory RDF" $
      readScalarString (getExample "single_page.xml") `shouldSatisfy` (\(Right g) -> isIsomorphic g singlePage)
  describe "parseScalar" $ do
    it "parses RDF from a simple book into Scalar" $
      parseScalar singlePage def `shouldBe` Right singlePageScalar
  describe "orderPages" $ do
    it "throws an error if a path is missing" $
      orderPages singlePageScalar `shouldSatisfy` (\(Left (ScalarError err)) -> "Could not find path" `isPrefixOf` err)
    it "gets all pages if `None` page order strategy is used" $
      orderPages singlePageScalar {scalarOptions = def {orderPagesBy = None}} `shouldBe` Right [singlePageScalarPage]
    it "returns a list of pages in order" $
      orderPages fullBookScalar `shouldSatisfy` (\(Right pages) -> length pages == 3)
