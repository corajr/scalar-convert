{-# LANGUAGE StandaloneDeriving #-}
module Text.Scalar.Types ( URI
                         , VersionURI
                         , unVersionURI
                         , mkVersionURI
                         , Path
                         , Page(..)
                         , Scalar(..)
                         , ScalarError(..)
                         ) where

import Text.Pandoc.Error (PandocError(..))
import Data.RDF (ParseFailure)

import qualified Data.Text as T

type URI = T.Text

newtype VersionURI = VersionURI { unVersionURI :: URI }
  deriving (Eq, Show)

mkVersionURI :: URI -> VersionURI
mkVersionURI = VersionURI

type Path = [VersionURI]

data Page = Page
  { pageVersionURI :: VersionURI
  , pageContent :: T.Text
  } deriving (Eq, Show)

data Scalar = Scalar
  { pages :: [Page]
  } deriving (Eq, Show)

deriving instance Eq PandocError

data ScalarError = ScalarError String
                 | RdfError ParseFailure
                 | FromPandoc PandocError
                 deriving (Eq, Show)
