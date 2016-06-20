{-# LANGUAGE TemplateHaskell #-}

module Text.ScalarSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.FileEmbed
import qualified Data.ByteString

import Text.Scalar

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

examples :: [(FilePath, Data.ByteString.ByteString)]
examples = $(embedDir "test/examples")

spec :: Spec
spec = do
  describe "parseScalarXML" $ do
    it "parses a Scalar RDF/XML string into in-memory RDF" $
      pending
