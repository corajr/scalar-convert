{-# LANGUAGE NamedFieldPuns #-}
module Text.Scalar.CLI where

import System.IO (hPutStr, hPutStrLn, hPrint, stderr)
import Control.Monad (when)

import Text.Scalar
import Text.Pandoc.Options
import Text.Pandoc.Readers.Scalar
import Text.Pandoc (Pandoc, writeJSON)

import qualified Data.Text as T
import Options.Applicative

data CliArgs = CliArgs
  { maybePath :: Maybe String
  , warnings :: Bool
  , inputFile :: String
  }

cliArgs :: Parser CliArgs
cliArgs = CliArgs
  <$> (optional $ strOption ( short 'p'
                           <> metavar "PATH"
                           <> help "The Scalar page with the path to follow (defaults to index)"))

  <*> switch ( short 'v'
             <> long "verbose"
             <> help "display additional processing information")
  <*> argument str (metavar "INPUT")

run :: CliArgs -> IO ()
run (CliArgs {inputFile, maybePath, warnings}) = do
  let orderBy = case maybePath of
        Just path -> Path $ T.pack path
        Nothing -> IndexPath
      scalarOpts = def {orderPagesBy = orderBy}
  (pandoc, log') <- fmap runScalarM $ case inputFile of
    "-" -> parseStdin scalarOpts
    _ -> readAndParseScalarFile inputFile scalarOpts
  when (not (null log') && warnings) $ do
    hPutStr stderr $ "Warnings:\n" ++ log'
    hPutStrLn stderr ""
  case pandoc of
    Left err -> hPutStr stderr "Error: " >> case err of
      ScalarError strErr -> hPutStrLn stderr strErr
      _ -> hPrint stderr err
    Right doc -> putStrLn $ writeJSON def doc

parseStdin :: ScalarOptions -> IO (ScalarM Pandoc)
parseStdin opts = getContents >>= return . readScalar def opts

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> cliArgs)
      ( fullDesc
     <> progDesc "Reads INPUT and outputs pandoc's native format to stdout"
     <> header "scalar-convert - export ANVC Scalar RDF/XML with pandoc" )
