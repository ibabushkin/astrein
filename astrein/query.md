# ASTrein Query Format
As mentioned in the main README, queries are made up of two components, `query
terms` and `query combinators`. The parsing machinery also exports default
patterns which are used by the language backends to treat similar concepts
across languages identically in queries. For example, toplevel values/functions
are query terms of the same kind (see their section for an explanation what
this means).

# Query Terms
Query terms are representing a single entity in an AST that is queried. They
are made up of two or more characters, which may not be spaces or parentheses.
The first character is split off and used as a separator, that is, the whole
term is split into chunks separated by the first character. For instance, the
string `,abc,def` gets parsed into a term tagged with the `,` separator and
containing two substrings, `abc` and `def`.

## Default Patterns

### Function/Value Names
Toplevel function and value declarations are matched by query terms with the
`.` separator, which only contain one word.

#### Examples
* `.funcName`
* `.value_name`

### Type Names
Type declarations of any kind are matched by query terms with the `:`
separator, which only contain one word.

#### Examples
* `:TypeName`

### Typeclass/Interface Names
Typeclass or interface declarations are matched by query terms with the `,`
separator, which contain only one word.

#### Examples
* `,ClassName`
* `,interface_name`

### Typeclass Instances/Interface implementations
Typeclass instance declarations or interface implementations are matched by
query terms with the `;` separator, which contain only two words.

#### Examples
* `;ClassName;TypeName`
* `;interface_name;implementor_name`

### Named Entities
Some language features are used rarely or are hard to allocate a separator
character for. In such cases, named query terms can be used. These use the
`/` separator, and contain two words, of which the first specifies the kind of
entity, and the second - it's name.

#### Examples
* `/export/TypeName`
* `/export/valueName`
* `/operator/.`

# Query Combinators
Query combinators allow for combining subqueries into larger queries. This is
done by interspersing subqueries with a combinator's separator character
surrounded by whitespace. Precedence is realized using parentheses (`()`).
These are mandatory, unless the nested combinator uses the same separator
character as it's parent. In such cases, the parentheses can be omitted and the
parser assumes the first combinator is the parent of the second one (and so on,
for arbitrary levels of nesting).

## Default Patterns

### Ranges
Ranges use the `-` separator. They match all declarations between the first
match of the first subquery and the last match of the second subquery,
inclusively.

#### Examples
* `.abc - .def` - match all declarations between the values `abc` and `def`

### Nesting
Nested queries restrict the search space for a subquery. They use the `.`
separator.

#### Examples
* `.abc . .def` - match the nested definition of `def` inside `abc`.

### Alternatives
These act as a logical `or` operator and use the `|` separator.
If the first subquery doesn't match, the second is tried on each declaration.

#### Examples
* `.abc | .def` - match all definitions of `abc` and `def`
