# Paradox Parser and Typechecker

Paradox is a front end for a language called Paradox which demonstrates type checking within a dynamically scoped language.

To build:

* Make sure you have Haskell Platform and *cabal version 1.18* installed. Paradox will NOT build on older versions of cabal.
* Install alex and happy (cabal install alex; cabal install happy)

```shell
# cabal configure --enable-tests
# cabal build
```

To run all the tests:

```shell
# cabal test
```

To run Paradox, you can do a "cabal install," and run it from your PATH or by referencing it from the current directory. There are plenty of examples in the examples/ directory.

```shell
Usage: paradox [-unparse | -ast] FILE
-unparse:       Display the UNPARSE of a program in FILE.
-ast:           Display the AST of program contained in FILE.

# dist/build/paradox/paradox -unparse examples/example1.pd
```

The -ast option will just display the AST along with a pretty printed AST for debugging purposes. 
