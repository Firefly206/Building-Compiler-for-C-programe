# Building a C Compiler

An OCaml implementation of a C compiler, built by following Nora Sandler's
*Writing a C Compiler* (No Starch Press).

## Structure

- `chapter_1/lexer/` — dune project for the chapter 1 lexer
- `my_tests/` — my own small C test programs
- `writing-a-c-compiler-tests/` — external test suite from
  [nlsandler/writing-a-c-compiler-tests](https://github.com/nlsandler/writing-a-c-compiler-tests)
  (cloned separately, not tracked in this repo)

## Status

| Stage                | State                                                        |
| -------------------- | ------------------------------------------------------------ |
| Lexer                | Done, passes the book's chapter 1 lex tests                  |
| Parser (AST)         | Done, passes the book's chapter 1 parse tests                |
| Tree printer         | Done, prints the AST with `--parse` and `--sem`              |
| Semantic analysis    | Done, unique variable names and scoping (`--sem`)            |
| IR generation        | Done, three-address steps with labels and jumps (`--ir`)     |
| Assembly generation  | Done, NASM Intel syntax (`--asm`)                            |
| Driver               | Done, `main.exe prog.c` builds an executable                 |

The parser is already bigger than chapter 1 needs. It also handles local
variables, assignment, `if`/`else`, `while`, `for`, blocks, and the binary
and logical operators from the later chapters. See "What the parser accepts".

## Requirements

- OCaml and dune (the project uses dune 3.24)
- `nasm` and `gcc` (gcc preprocesses the source and links the result)
- Linux or macOS

## Build

From this folder:

```
dune build
```

The program is built to `_build/default/bin/main.exe`.

## Run

To compile a C file into an executable, give only the file:

```
./_build/default/bin/main.exe path/to/prog.c
path/to/prog
```

The driver runs `gcc -E -P` (preprocess), then the compiler stages, then
`nasm -f elf64` and `gcc` (assemble and link). It leaves the executable next
to the source file and deletes the temporary `.i`, `.asm` and `.o` files, even
when the compile fails.

To look at one stage, give a flag first and the file last:

```
./_build/default/bin/main.exe --lex   path/to/file.c
./_build/default/bin/main.exe --parse path/to/file.c
./_build/default/bin/main.exe --sem   path/to/file.c
./_build/default/bin/main.exe --ir    path/to/file.c
./_build/default/bin/main.exe --asm   path/to/file.c
```

- `--lex` prints the list of tokens, one per line.
- `--parse` prints the AST as an indented tree.
- `--sem` prints the AST after semantic analysis (variables renamed).
- `--ir` prints the intermediate representation.
- `--asm` prints the NASM assembly to stdout.
- If the program is not valid, the compiler fails with an error message and
  a non-zero exit code.

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
With `--sem`, the same tree shows `a.0` where it shows `a`.

## Tests

The book's test suite is a separate repository (`writing-a-c-compiler-tests`).
With it next to this project, run these from this folder:

```ocaml
dune build
../../writing-a-c-compiler-tests/test_compiler $(pwd)/_build/default/bin/main.exe --chapter 1 --stage lex
../../writing-a-c-compiler-tests/test_compiler $(pwd)/_build/default/bin/main.exe --chapter 1 --stage parse
../../writing-a-c-compiler-tests/test_compiler $(pwd)/_build/default/bin/main.exe --chapter 1
```

All three should end with `OK`. The last one has no `--stage`: it compiles
every valid program with the driver, runs it and checks the exit code. The
invalid programs in the suite must be rejected.

Only chapter 1 is expected to pass for now. Later chapters need things this
compiler does not have yet: function calls and prototypes, and the tokens
`~`, `%`, `?` and `:`.

My own test programs are in `my_tests/`, including the semantic tests
(`sem_shadow.c`, `sem_undeclared.c`, `sem_mytest1.c` to `sem_mytest3.c`).

## Project layout

```
bin/main.ml        command line: the driver and the --lex/--parse/--sem/--ir/--asm flags
lib/lexer.ml       source text -> list of tokens
lib/ast.ml         the AST types (what the parser builds)
lib/parser.ml      list of tokens -> AST
lib/semantic.ml    AST -> AST with unique variable names, checks scopes
lib/ir.ml          AST -> IR (three-address steps)
lib/asm.ml         IR -> assembly -> NASM text
lib/pretty.ml      AST -> indented tree text, used by --parse and --sem
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

**Semantic analysis (`semantic.ml`).** Gives every declared variable a unique
name (`x` becomes `x.0`, `x.1`, ...) and rewrites every use to match. A map
from the C name to its unique name is passed along; each entry remembers
whether it was declared in the current block. Declaring the same name twice
in one block is an error, shadowing an outer variable is allowed, and using
a variable that was never declared is an error. Leaving a block simply goes
back to the older map. After this stage the IR does not need to know about
scopes.

**IR (`ir.ml`).** Flattens the tree into a list of simple steps (copy, unary,
binary, jump, jump if zero, label, return). Intermediate results go into
temporaries named `tmp.N` and labels are named `name_N`, with one counter
passed along. `&&`, `||`, `if`, `while` and `for` become jumps and labels.
Every function ends with an extra `return 0`, so a function that falls off
the end still returns (and `main` returns 0, as C requires).

**Assembly (`asm.ml`).** Turns each IR step into x86-64 instructions in NASM
Intel syntax. Variables first live in pseudoregisters, then each one is
given a stack slot. A fix-up pass rewrites instructions x86 does not allow
(like a `mov` between two memory operands) using scratch registers. Relational
operators use `cmp` and `setcc`, division uses `idiv`.

**Driver (`main.ml`).** With one argument it runs the whole pipeline: `gcc -E -P`,
the stages above, `nasm -f elf64`, then `gcc` to link.

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
  (a `void` function also gets the extra `return 0` for now).
- No `~`, `%`, `?:`, `do`/`while`, `break`, `continue` or `switch` yet.
- A user variable named `tmp` can clash with an IR temporary name.
- Errors are plain `failwith` messages, so they show up as an uncaught
  exception.
