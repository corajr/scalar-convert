{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Readers.ScalarSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Text.Scalar (parseScalar)
import Text.Scalar.Types
import Text.Pandoc.Readers.Scalar

import Text.Pandoc
import Text.Pandoc.Error

import Data.Either (isRight)

import Text.ScalarSpec (singlePageScalar)
import Examples

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pageToBlocks" $ do
    it "takes a 'Page' and returns Right '[Block]'" $
      pageToBlocks def singlePageScalarPage `shouldBe` Right (singlePageTitle : singlePageContentPandoc)
  describe "scalarToPandoc" $ do
    it "takes a 'Scalar' book and returns 'Pandoc'" $ do
      scalarToPandoc def singlePageScalar { scalarOptions = def { orderPagesBy = None }} `shouldBe` Right singlePagePandoc
  describe "readScalar" $ do
    it "parses a Scalar RDF/XML string into Right 'Pandoc'" $
      readScalar def def {orderPagesBy = None } (getExample "single_page.xml") `shouldBe` Right singlePagePandoc
    it "parses a complex Scalar RDF/XML string into Right 'Pandoc'" $
      readScalar def def (getExample "full_book.xml") `shouldSatisfy` isRight
