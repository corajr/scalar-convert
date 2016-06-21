{-# LANGUAGE NamedFieldPuns #-}
module Text.Scalar.CLI where

import Text.Pandoc.Options
import Text.Pandoc.Readers.Scalar
import Text.Pandoc.Writers.Native

import qualified Data.Text as T
import Options.Applicative

data CliArgs = CliArgs
  { input :: FilePath
  , maybeURI :: Maybe String
  }

cliArgs :: Parser CliArgs
cliArgs = CliArgs
  <$> argument str (metavar "INPUT")
  <*> (optional $ argument str (metavar "STARTURI"))

run :: CliArgs -> IO ()
run (CliArgs {input, maybeURI}) = do
  pandoc <- readAndParseScalarFile input (fmap T.pack maybeURI)
  case pandoc of
    Left err -> print err
    Right doc -> putStr $ writeNative def doc

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> cliArgs)
      ( fullDesc
     <> progDesc "Reads INPUT and outputs pandoc's native format to stdout"
     <> header "scalar-export - convert ANVC Scalar RDF/XML with pandoc" )
