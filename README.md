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
  query processing machinery. This property, however, can be interpreted freely
  depending on language context. For example, when searching a Haskell codebase
  for a toplevel definition, you can distinguish types and values. These also
  need a distinction in queries. However, further distinction between normal
  bindings and constructors is not necessary due to naming rules enforced by
  the language itself. Similar cases arise around `newtype` declarations and
  type families. The general rule of thumb is to distinguish only the entities
  the user can distinguish when encountering them in a codebase and searching
  for their origin. Call this `duck typing` or whatever you like, but it is the
  most powerful approach I am aware of.
* A codebase in a language I enjoy writing in, and which allows me to separate
  the business logic from the I/O plumbing work needed in a more rigorous way.

The result of this ideas and requirements is ASTrein - a command line tool
parsing source code into an AST representation and querying that for names of
entities, with a notion of what those entities *are*. Support for more advanced
selectors in the spirit of `cq` is also being worked on, but these bits will
probably undergo more serious changes in the future.

Currently, an implementation for Haskell code is present (and works quite
well), but the main focus is on providing a framework for support of a large
set of languages, and interaction with user-provided extension modules. This
would allow to build up query tooling for almost all kinds of structured
textual data, like configuration files, some plaintext data formats like `YAML`
or `JSON`. Granted, those formats have their own tooling available, but
sometimes being able to reuse the same querying syntax is of great value.

## Features
ASTrein supports two main query components: `query terms` and `query
combinators`. These make up the query parsing primitives used to build up a
simple AST of the query, which is then passed to the various language backends
that verify it for correctness - that is, check whether the passed raw query
"makes sense". This system allows for multiple standardized query terms
representing types, values, typeclasses/interfaces, as well as standardized
query combinators representing nesting, ranges and alternatives. The various
backends interpret those loosely and according to their own context, but the
parsing process is shared and well-defined. For information on the grammar, see
the [query documentation](https://github.com/ibabushkin/tree/master/astrein/query.md).
If you need help on the various backends' implementations, see their respective
query feature docs:

* [Haskell]()

## Usage
The main executable, `astrein` can be used to invoke subcommands, which itself
are dispatched to executables with names of the form `astrein-subcommand`.

To see help messages:
```sh
$ astrein --help
astrein 0.4.0.0
USAGE: astrein [SUBCOMMAND [SUBCOMMAND-OPTION(S)] FILE(S)|OPTION(S)]
OPTIONS:
  -l  --list, --languages  List available subcommands/languages instead of calling a subcommand.
  -h  --help               Show this help.

$ astrein haskell --help
astrein-haskell 0.6.1.0
USAGE: astrein-haskell [OPTION(S)] FILE(S)
OPTIONS:
  -q QUERY  --query=QUERY  The query the AST is to be matched on.
                           If no such argument is present, dump the AST instead of
                           matching a query.
  -h        --help         Show this help.
  -v        --verbose      If passed, more (some) output for non-matches is generated.
```

# TODO
See [issues](https://github.com/ibabushkin/astrein/issues).
