{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Scalar.Types ( URI
                         , VersionURI
                         , unVersionURI
                         , mkVersionURI
                         , Path
                         , Page(..)
                         , Scalar(..)
                         , ScalarError(..)
                         , FindPagesBy(..)
                         , ScalarOptions(..)
                         ) where

import Text.Pandoc.Error (PandocError(..))
import Data.RDF (ParseFailure)

import Data.Default

import qualified Data.Text as T

type URI = T.Text

newtype VersionURI = VersionURI { unVersionURI :: URI }
  deriving (Eq, Show)

mkVersionURI :: URI -> VersionURI
mkVersionURI uri = VersionURI preFragment
  where preFragment = T.takeWhile (/= '#') uri

type Path = [VersionURI]

data Page = Page
  { pageVersionURI :: VersionURI
  , pageTitle :: T.Text
  , pageContent :: T.Text
  } deriving (Eq, Show)

data Scalar = Scalar
  { scalarPages :: [Page]
  } deriving (Eq, Show)

deriving instance Eq PandocError

data ScalarError = ScalarError String
                 | RdfError ParseFailure
                 | FromPandoc PandocError
                 deriving (Eq, Show)

data FindPagesBy = IndexPath
                 | Path URI
                 | GetAll
                 deriving (Eq, Show)

data ScalarOptions = ScalarOptions
  { findPagesBy :: FindPagesBy
  } deriving (Eq, Show)

instance Default ScalarOptions where
  def = ScalarOptions { findPagesBy = IndexPath
                      }
