{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples where

import Data.RDF
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.FileEmbed
import qualified Data.ByteString.Char8 as BS
import Text.Pandoc.Definition

import Data.Default (def)
import Text.Scalar

import Test.Hspec (Expectation, shouldBe, shouldSatisfy)

examples :: [(FilePath, BS.ByteString)]
examples = $(embedDir "test/examples")

getExample :: FilePath -> String
getExample ex = case (lookup ex examples) of
  Just s -> BS.unpack s
  Nothing -> error $ ex ++ " not found"

singlePage :: HashMapS
singlePage = mkRdf triples baseurl prefixes
  where baseurl = Just (BaseUrl "")
        prefixes = PrefixMappings $ Map.fromList [ ("dcterms", "http://purl.org/dc/terms/")
                                                 , ("ov", "http://open.vocab.org/terms/")
                                                 , ("prov", "http://www.w3.org/ns/prov#")
                                                 , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                                                 , ("scalar", "http://scalar.usc.edu/2012/01/scalar-ns#")
                                                 , ("sioc", "http://rdfs.org/sioc/ns#") ]
        triples = [ Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index") (UNode "scalar:citation") (LNode (PlainL "method=instancesof/content;methodNumNodes=1;"))
                  , Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index") (UNode "dcterms:hasVersion") (UNode "http://scalar.usc.edu/works/scalar-export-test/index.1")
                  , Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index") (UNode "scalar:version") (UNode "http://scalar.usc.edu/works/scalar-export-test/index.1"),Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index") (UNode "scalar:urn") (UNode "urn:scalar:content:297474")
                  , Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index") (UNode "dcterms:created") (LNode (PlainL "2016-06-20T12:41:52-07:00"))
                  , Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index") (UNode "prov:wasAttributedTo") (UNode "http://scalar.usc.edu/works/scalar-export-test/users/11802")
                  , Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index") (UNode "scalar:isLive") (LNode (PlainL "1"))
                  , Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index") (UNode "rdf:type") (UNode "http://scalar.usc.edu/2012/01/scalar-ns#Composite")
                  , Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index.1") (UNode "rdf:type") (UNode "http://scalar.usc.edu/2012/01/scalar-ns#Version")
                  , Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index.1") (UNode "dcterms:isVersionOf") (UNode "http://scalar.usc.edu/works/scalar-export-test/index")
                  , Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index.1") (UNode "scalar:urn") (UNode "urn:scalar:version:791282")
                  , Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index.1") (UNode "dcterms:created") (LNode (PlainL "2016-06-20T12:41:52-07:00"))
                  , Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index.1") (UNode "prov:wasAttributedTo") (UNode "http://scalar.usc.edu/works/scalar-export-test/users/11802")
                  , Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index.1") (UNode "scalar:defaultView") (LNode (PlainL "plain"))
                  , Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index.1") (UNode "sioc:content") (LNode (PlainL "This is a test book for the <a href=\"https://github.com/corajr/scalar-export\">scalar-export</a>&nbsp;package. It contains different formatting, such as <strong>bold</strong> and&nbsp;<em>italics</em>."))
                  , Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index.1") (UNode "dcterms:title") (LNode (PlainL "Introduction"))
                  , Triple (UNode "http://scalar.usc.edu/works/scalar-export-test/index.1") (UNode "ov:versionnumber") (LNode (PlainL "1"))
                  ]

singlePageContent :: T.Text
singlePageContent = "This is a test book for the <a href=\"https://github.com/corajr/scalar-export\">scalar-export</a>&nbsp;package. It contains different formatting, such as <strong>bold</strong> and&nbsp;<em>italics</em>."

singlePageContentPandoc :: [Block]
singlePageContentPandoc = [Para [Str "This",Space,Str "is",Space,Str "a",Space,Str "test",Space,Str "book",Space,Str "for",Space,Str "the",Space,Link ("",[],[]) [Str "scalar-export"] ("https://github.com/corajr/scalar-export",""),Str "\160package.",Space,Str "It",Space,Str "contains",Space,Str "different",Space,Str "formatting,",Space,Str "such",Space,Str "as",Space,Strong [Str "bold"],Space,Str "and\160",Emph [Str "italics"],Str "."]]

singlePageTitle :: Block
singlePageTitle = Header 1 ("introduction", [], []) [Str "Introduction"]

singlePagePandoc :: Pandoc
singlePagePandoc = Pandoc nullMeta (singlePageTitle : singlePageContentPandoc)

singlePageScalarPage :: Page
singlePageScalarPage = Page { pageTitle = "Introduction", pageContent = singlePageContent }

singlePageScalar :: Scalar
singlePageScalar =
  Scalar { scalarOptions = def
         , scalarPaths = Map.empty
         , scalarPages = Map.singleton singlePageVersionURI singlePageScalarPage
         }

indexURI :: URI
indexURI = "http://scalar.usc.edu/works/scalar-export-test/index"

singlePageVersionURI :: VersionURI
singlePageVersionURI = mkVersionURI $ indexURI `mappend` ".1"

fullBookRdf :: HashMapS
fullBookRdf =
  case fst (runScalarM (readScalarString (getExample "full_book.xml"))) of
    Left err -> error (show err)
    Right x -> x

fullBookScalar :: Scalar
fullBookScalar = case runScalarM (parseScalar fullBookRdf def) of
  (Left err, _) -> error (show err)
  (Right x, _) -> x

shouldBeScalar :: (Eq a, Show a) => ScalarM a -> Either ScalarError a -> Expectation
action `shouldBeScalar` result = fst (runScalarM action) `shouldBe` result

shouldSatisfyScalar :: (Show a) => ScalarM a -> (Either ScalarError a -> Bool) -> Expectation
action `shouldSatisfyScalar` predicate = fst (runScalarM action) `shouldSatisfy` predicate
