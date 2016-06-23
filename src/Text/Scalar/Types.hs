{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Scalar.Types ( URI
                         , VersionURI
                         , mkVersionURI
                         , unVersionURI
                         , PathID
                         , mkPathID
                         , unPathID
                         , Path
                         , PathResourceURI
                         , PathBodyURI
                         , PathTargetURI
                         , PathComponent(..)
                         , Page(..)
                         , ScalarM
                         , runScalarM
                         , Scalar(..)
                         , ScalarError(..)
                         , PageOrderStrategy(..)
                         , ScalarOptions(..)
                         ) where

import Text.Pandoc.Error (PandocError(..))
import Data.RDF (ParseFailure)

import Data.Default
import Data.Map (Map)

import Control.Monad.Except
import Control.Monad.Writer.Strict

import qualified Data.Text as T

type URI = T.Text

newtype VersionURI = VersionURI { unVersionURI :: URI }
  deriving (Eq, Show, Ord)

mkVersionURI :: URI -> VersionURI
mkVersionURI uri = VersionURI preFragment
  where preFragment = T.takeWhile (/= '#') uri

newtype PathID = PathID { unPathID :: T.Text }
  deriving (Eq, Show, Ord)

mkPathID :: T.Text -> PathID
mkPathID = PathID

type Path = [VersionURI]

data PathComponent = PathComponent
  { pathIndex :: Int
  , pathVersionURI :: VersionURI
  } deriving (Eq, Show, Ord)

type PathResourceURI = URI
type PathBodyURI = URI
type PathTargetURI = URI

data Page = Page
  { pageTitle :: T.Text
  , pageContent :: T.Text
  } deriving (Eq, Show)

data PageOrderStrategy = IndexPath
                       | Path URI
                       | None
                       deriving (Eq, Show)

type ScalarM = ExceptT ScalarError (Writer String)

data ScalarOptions = ScalarOptions
  { orderPagesBy :: PageOrderStrategy
  } deriving (Eq, Show)

instance Default ScalarOptions where
  def = ScalarOptions { orderPagesBy = IndexPath
                      }

deriving instance Eq PandocError

data ScalarError = ScalarError String
                 | RdfError ParseFailure
                 | FromPandoc PandocError
                 deriving (Eq, Show)

data Scalar = Scalar
  { scalarOptions :: ScalarOptions
  , scalarPaths :: Map PathID Path
  , scalarPages :: Map VersionURI Page
  } deriving (Eq, Show)

runScalarM :: ScalarM a -> (Either ScalarError a, String)
runScalarM = runWriter . runExceptT
