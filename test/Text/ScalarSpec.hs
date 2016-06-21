module Text.ScalarSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.RDF
import Data.Either (isRight)

import Examples (getExample, singlePage)

import Text.Scalar

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "readScalarString" $ do
    it "parses a Scalar RDF/XML string into in-memory RDF" $
      readScalarString (getExample "single_page.xml") `shouldSatisfy` (\(Right g) -> isIsomorphic g singlePage)
  describe "" $ do
    it "" $
      pending
