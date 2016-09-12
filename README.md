# ASTrein - an AST querying library and command line tool
At some point in time I stumbled across a JavaScript tool intended to
selectively display source code: [`cq`](https://github.com/fullstackio/cq).  It
had some drawbacks for my usecases, however: it only supports JavaScript, which
I don't write in, and seems to be focused on embedding source code in papers,
blogposts and the like. So I quickly decided that something similar should have
a few properties to fit my needs, which mainly revolve around searching
larger codebases for definitions of some entity:

* Extensibility. It should be possible to extend the tool for language
  support etc.
* Type-Safety and more semantic information in the queries: It should be
  possible to search for things knowing *what* they are in addition to what
  they are *called*. This quickly leads to a demand for type-safety in the
  query processing machinery.
* A codebase in a language I enjoy writing in, and which allows me to separate
  the business logic from the I/O plumbing work needed more rigorously.

The result of this ideas and requirements is ASTrein - a command line tool
parsing source code into an AST representation and querying that for names of
entities. Support for more advanced selectors in the spirit of `cq` is also
being worked on, but these bits will probably undergo more serious changes in
the future.

Currently, a proof-of-concept implementation for Haskell code is present (and
works quite well), but the main focus is on providing a framework for support
of a large set of languages, and interaction with user-provided extension
modules. This would allow to build up query tooling for almost all kinds of
structured textual data.

## Usage
The main executable, `astrein` can be used to invoke subcommands, which itself
are dispatched to executables with names of the form `astrein-subcommand`. An
`astrein-haskell` executable is provided as well. At the moment, only a handful
of query types are supported by that backend:

* `:Type` queries for type `Type`.
* `.name` queries for a toplevel binding `name`.
* `,Class` queries for a typeclass `name`.
* `;Class;Type` queries for a `Class` instance of type `Type`.

An additional combinator ` - ` is provided, to search for ranges, so
`:Type1 - :Type2` will query for the inclusive range between the first and the
second type's definition.

Check out `astrein --help` and `astrein haskell --help` for information on the
command line tools' usage itself.

# TODO
* [ ] a proper README
* [ ] maybe an improved framework for plugins
* [ ] more plugins
