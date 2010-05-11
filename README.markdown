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


## functionExtractorMap ##

functionExtractorMap works like functionsExtractor but applies a function over all function-pairs.

This functions is useful if the common return type of the functions is a type class.

### signature

    functionExtractorMap :: String -> ExpQ -> ExpQ

### example

    secondTypeclassTest =
      do let expected = ["45", "88.8", "\"hej\""]
             actual = $(functionExtractorMap "^tc" [|\n f -> show f|] )
         expected @=? actual
    
    tcInt :: Integer
    tcInt = 45
    
    tcDouble :: Double
    tcDouble = 88.8
    
    tcString :: String
    tcString = "hej"


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
