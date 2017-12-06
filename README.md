# Edsger
A compiler implementation of Edsger programming language.

This was a project for the Compilers course in NTUA (2015-2016).
The compiler is implemented in OCaml and is based on LLVM infrastructure.
For further information refer to edsger2016.pdf file (in greek unfortunately).

## Requirements
* opam 4.03
* llvm 3.5

## Installation

Change paths in Makefile.
* make all : Compiles the project
* make clean : Removes object files except for the executable of the compiler
* make distclean : Removes everything

The compiler executable will be copied in the testcases directory.

## Standard Library

The library located here: https://github.com/abenetopoulos/edsger_lib was used. Follow the instructions provided there to install 
the library. Execute lib.sh and copy lib.a in the testcases folder if not already there.

## Usage

Execution of ./compiler [-f] [-i] [--ast] <source>.eds results <source>.ll file

flags:
* -f : reads source from stdin, ouputs final IR (with optimizations) in stdout.
* -i : reads source from stidn, outputs IR (without optimizations) in stdout.
* --ast : prints the Abstract Syntax Tree (AST).

Headers should be in the same directory with source code. If you wish to load them automatically use Lexer2.mll not Lexer.mll.

## Full compilation

Use Edsger.sh [-f] [-i] [-0] [--ast] <source>.eds as it has already been described.
