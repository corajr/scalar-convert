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
import Examples (getExample, singlePage, singlePageContent, singlePageTitle, singlePageContentPandoc)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

singlePagePandoc :: Pandoc
singlePagePandoc = Pandoc nullMeta (singlePageTitle : singlePageContentPandoc)

page :: Page
page = Page { pageTitle = "Introduction", pageContent = singlePageContent }

spec :: Spec
spec = do
  describe "pageToBlocks" $ do
    it "takes a 'Page' and returns Right '[Block]'" $
      pageToBlocks def page `shouldBe` Right (singlePageTitle : singlePageContentPandoc)
  describe "scalarToPandoc" $ do
    it "takes a 'Scalar' book and returns 'Pandoc'" $ do
      scalarToPandoc def singlePageScalar `shouldBe` Right singlePagePandoc
  describe "readScalar" $ do
    it "parses a Scalar RDF/XML string into Right 'Pandoc'" $
      readScalar def (getExample "single_page.xml") `shouldBe` Right singlePagePandoc
    it "parses a complex Scalar RDF/XML string into Right 'Pandoc'" $
      readScalar def (getExample "full_book.xml") `shouldSatisfy` isRight
