{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Readers.ScalarSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Text.Scalar.Types
import Text.Pandoc.Readers.Scalar

import Text.Pandoc
import Text.Pandoc.Builder

import Data.Either (isRight)
import qualified Data.Map as Map

import Text.ScalarSpec (singlePageScalar)
import Examples

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

inlineHead :: Many Inline -> Inline
inlineHead = head . toList

noteSpan :: Inline
noteSpan = Span ("",["note"],[("rev","scalar:has_note"),("resource","note-on-notes")]) [Str "notes"]

processedNoteSpan :: Inline
processedNoteSpan = Span ("",[],[]) [Str "notes",Note [Para [Str "this"]]]

notePage :: Page
notePage = Page { pageTitle = "Note on notes"
                , pageContent = "this"
                }

noteScalar :: Scalar
noteScalar =
  Scalar { scalarPages = Map.singleton (mkVersionURI "/note-on-notes.1") notePage
         , scalarPaths = Map.empty
         , scalarOptions = def
         }

spec :: Spec
spec = do
  describe "notesTransform" $ do
    it "turns a span class='note' into a pandoc Note" $
      notesTransform noteScalar noteSpan `shouldBe` processedNoteSpan
  describe "pageToBlocks" $ do
    it "takes a 'Page' and returns Right '[Block]'" $
      pageToBlocks def singlePageScalarPage `shouldBeScalar` Right (singlePageTitle : singlePageContentPandoc)
  describe "scalarToPandoc" $ do
    it "takes a 'Scalar' book and returns 'Pandoc'" $ do
      scalarToPandoc def singlePageScalar { scalarOptions = def { orderPagesBy = None }} `shouldBeScalar` Right singlePagePandoc
  describe "readScalar" $ do
    it "parses a Scalar RDF/XML string into Right 'Pandoc'" $
      readScalar def def {orderPagesBy = None } (getExample "single_page.xml") `shouldBeScalar` Right singlePagePandoc
    it "parses a complex Scalar RDF/XML string into Right 'Pandoc'" $
      readScalar def def (getExample "full_book.xml") `shouldSatisfyScalar` isRight
