# Building a C Compiler

An OCaml implementation of a C compiler, built by following Nora Sandler's
*Writing a C Compiler* (No Starch Press).

## Status

Chapter 1 — lexer + parser for a minimal C subset: programs of the form
`int main(void) { return <int>; }`. Covers keyword/identifier/integer-literal
tokenization, whitespace handling, and rejecting invalid tokens or malformed
structure (see `writing-a-c-compiler-tests/tests/chapter_1/`).

## Structure

- `chapter_1/lexer/` — dune project for the chapter 1 lexer
- `writing-a-c-compiler-tests/` — external test suite from
  [nlsandler/writing-a-c-compiler-tests](https://github.com/nlsandler/writing-a-c-compiler-tests)
  (cloned separately, not tracked in this repo)

## Building
