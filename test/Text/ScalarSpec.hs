{-# LANGUAGE OverloadedStrings #-}
module Text.ScalarSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Default (def)
import Data.RDF

import Examples (getExample, singlePage, singlePageContent)

import Text.Scalar

import Text.Scalar.RDFSpec (versionURI)

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
      parseScalar singlePage def `shouldBe` Right (Scalar [Page versionURI "Introduction" singlePageContent])
