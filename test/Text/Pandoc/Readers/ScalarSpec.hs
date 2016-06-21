{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Readers.ScalarSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Text.Scalar (parseScalar)
import Text.Scalar.Types (Page(..), mkVersionURI)
import Text.Pandoc.Readers.Scalar

import Text.Pandoc
import Text.Pandoc.Error

import Data.Either (isRight)

import Examples (getExample, singlePage, singlePageContent, singlePageContentPandoc)

deriving instance Eq PandocError

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

singlePagePandoc :: Pandoc
singlePagePandoc = Pandoc nullMeta singlePageContentPandoc

page :: Page
page = Page { pageVersionURI = mkVersionURI "", pageContent = singlePageContent }

spec :: Spec
spec = do
  describe "pageToBlocks" $ do
    it "takes a 'Page' and returns Right '[Block]'" $
      pageToBlocks def page `shouldBe` Right singlePageContentPandoc
  describe "scalarToPandoc" $ do
    it "takes a 'Scalar' book and returns 'Pandoc'" $
      scalarToPandoc def (parseScalar singlePage Nothing) `shouldBe` Right singlePagePandoc
  describe "readScalar" $ do
    it "parses a Scalar RDF/XML string into Right 'Pandoc'" $
      readScalar def (getExample "single_page.xml") `shouldBe` Right singlePagePandoc
    it "parses a complex Scalar RDF/XML string into Right 'Pandoc'" $
      readScalar def (getExample "full_book.xml") `shouldSatisfy` isRight
