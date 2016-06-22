# scalar-convert

scalar-convert takes an RDF/XML exported from
[ANVC Scalar](https://github.com/anvc/scalar) as input, allowing you to create a
linear document using [pandoc](http://pandoc.org/).

## Installation

scalar-convert requires pandoc in order to produce useful output. On OS X, you
can install pandoc with [Homebrew](http://brew.sh/) by running `brew install
pandoc` in the terminal.

To build, you must first install
[stack](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md)
(on OS X with Homebrew, this can be done with `brew install haskell-stack`) and
then run the following:

```sh
git clone https://github.com/corajr/scalar-convert.git
cd scalar-convert
stack install
```

## Usage

First, export your book as RDF/XML from the link on the Import/Export tab of
your Scalar Dashboard. Save the resulting file name as `content.xml` in a
convenient directory, then open that directory in the terminal and run:

`scalar-export content.xml | pandoc -f native -o output.html`
