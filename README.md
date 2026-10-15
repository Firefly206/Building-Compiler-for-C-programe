# Building a C Compiler

An OCaml implementation of a C compiler, built by following Nora Sandler's
*Writing a C Compiler* (No Starch Press).

## Structure

- `chapter_1/lexer/` — dune project for the chapter 1 lexer
- `writing-a-c-compiler-tests/` — external test suite from
  [nlsandler/writing-a-c-compiler-tests](https://github.com/nlsandler/writing-a-c-compiler-tests)
  (cloned separately, not tracked in this repo)

## Status

| Stage                | State                                          |
| -------------------- | ---------------------------------------------- |
| Lexer                | Done, passes the book's chapter 1 lex tests    |
| Parser (AST)         | Done, passes the book's chapter 1 parse tests  |
| Tree printer         | Done, prints the AST with `--parse`            |
| Semantic analysis    | Not started                                    |
| IR generation        | Not started                                    |
| Assembly generation  | Not started                                    |
| Code emission        | Not started                                    |

The parser is already bigger than chapter 1 needs. It also handles local
variables, assignment, `if`/`else`, `while`, `for`, blocks, and the binary
and logical operators from the later chapters. See "What the parser accepts".

## Requirements

- OCaml and dune (the project uses dune 3.24)
- Linux or macOS

## Build

From this folder:

```
dune build
```

The program is built to `_build/default/bin/main.exe`.

## Run

The flag comes first and the file comes last:

```
./_build/default/bin/main.exe --lex   path/to/file.c
./_build/default/bin/main.exe --parse path/to/file.c
```

- `--lex` prints the list of tokens, one per line.
- `--parse` prints the AST as an indented tree.
- If the program is not valid, the compiler fails with an error message and
  a non-zero exit code.

Neither flag writes any output file, which is what the book's test script
expects.

Example. This file:

```c
int main(void) {
    int a = 1 + 2 * 3;
    return -a;
}
```

gives this tree with `--parse`:

```
Program
  Function main : int
    Parameter
    Body
      Declaration a : int
        Binary( Add )
          Constant(1)
          Binary( Multiply )
            Constant(2)
            Constant(3)
      Return
        Unary( Negate )
          Variable(a)
```

Notice that `Multiply` is under `Add`, because `*` binds tighter than `+`.

## Tests

The book's test suite is a separate repository (`writing-a-c-compiler-tests`).
With it next to this project, run these from this folder:

```
dune build
../../writing-a-c-compiler-tests/test_compiler $(pwd)/_build/default/bin/main.exe --chapter 1 --stage lex
../../writing-a-c-compiler-tests/test_compiler $(pwd)/_build/default/bin/main.exe --chapter 1 --stage parse
```

Both should end with `OK`. The invalid programs in the suite must be
rejected, so a failing test there usually means the lexer or parser accepted
something it should not have.

Only chapter 1 is expected to pass for now. Later chapters need things this
compiler does not have yet: function calls and prototypes, and the tokens
`~`, `%`, `?` and `:`.

## Project layout

```
bin/main.ml       command line: reads the file, runs --lex or --parse
lib/lexer.ml      source text -> list of tokens
lib/ast.ml        the AST types (what the parser builds)
lib/parser.ml     list of tokens -> AST
lib/pretty.ml     AST -> indented tree text, used by --parse
```

Later stages will also each get their own file/moule

## How each stage works

**Lexer (`lexer.ml`).** Reads the source as one string and cuts it into
tokens: keywords, names, numbers, punctuation and operators. Whitespace and
comments (`//` and `/* */`) are skipped. `=`, `!`, `<` and `>` look one
character ahead to tell `=` from `==` and so on. It rejects an unknown
character, a lone `&` or `|`, an unterminated block comment, and a number
directly followed by a letter or underscore (like `1foo`).

**Parser (`parser.ml`).** Recursive descent: one function per grammar rule.
Each function takes a token list and returns the tree it built plus the
tokens that are left over, so no mutable state is needed. Expressions use
precedence climbing: `parse_exp` takes a minimum priority and only takes
operators that are at least that strong. Assignment groups to the right
(`a = b = c` is `a = (b = c)`) and every other operator groups to the left.

**Tree printer (`pretty.ml`).** Every AST node is turned into a small
`{ label; children }` record, and one function prints that record with two
spaces of indent per level.

## What the parser accepts

```
<program>     ::= { <function> }
<function>    ::= <type> <identifier> ( <param-list> ) <block>
<param-list>  ::= void | <parameter> { , <parameter> }
<parameter>   ::= int <identifier>
<block>       ::= { { <block-item> } }
<block-item>  ::= <statement> | <declaration>
<declaration> ::= int <identifier> [ = <exp> ] ;
<statement>   ::= return <exp> ;
                | <exp> ;
                | if ( <exp> ) <statement> [ else <statement> ]
                | while ( <exp> ) <statement>
                | for ( <for-init> [ <exp> ] ; [ <exp> ] ) <statement>
                | <block>
                | ;
```

Operators, from tightest to loosest: unary `-` `!` (and `+`, which is
dropped), then `*` `/`, then `+` `-`, then `<` `<=` `>` `>=`, then `==` `!=`,
then `&&`, then `||`, then `=`.

## Known limits

- No function prototypes and no function calls.
- `return;` with no value is rejected.
- Only `int` variables and parameters. A function may return `int` or `void`.
- No `~`, `%`, `?:`, `do`/`while`, `break`, `continue` or `switch` yet.
- Errors are plain `failwith` messages, so they show up as an uncaught
  exception. The exit code is still non-zero.
