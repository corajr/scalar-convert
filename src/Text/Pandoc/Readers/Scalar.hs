module Text.Pandoc.Readers.Scalar ( readScalar ) where

import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Options

-- | Read Scalar RDF/XML from an input string and return a Pandoc document.
readScalar :: ReaderOptions -- ^ Reader options
           -> String        -- ^ String to parse (assuming @'\n'@ line endings)
           -> Either PandocError Pandoc
readScalar opts s =
  undefined
