# scalar-export

scalar-export takes an RDF/XML exported from
[ANVC Scalar](https://github.com/anvc/scalar) as input, producing a document
using [pandoc](http://pandoc.org/).

## Usage

`scalar-export content.xml | pandoc -f native -o output.html`
