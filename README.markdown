# template-helper

A small Haskell-module for extracting source code powered by Template Haskell.

## functionExtractor

### signature

    functionExtractor :: String -> ExpQ -- ListE

### usage

    testFunctions = $(functionExtractor "^test")

### example

    fooFunctionsInFile = $(functionExtractor "^foo")
    fooBar = "hej"
    fooGoo = "nej"

is the same as

    fooFunctionsInFile = [("fooBar", fooBar),("fooGoo", fooGoo)]
    fooBar = "hej"
    fooGoo = "nej"

## locationModule

### signature

    locationModule :: ExpQ -- LitE

### usage

    name = $(locationModule)

### example

    module Foo where
    name = $(locationModule)

is the same as

    module Foo where
    name = "Foo"

Useful with splice (`[|..|]`).
