{-# OPTIONS_GHC -XTemplateHaskell #-}
module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Language.Haskell.Extract 
import Language.Haskell.LocationTest
import Language.Haskell.ExtensionTest

main = defaultMain groupsOfTest

groupsOfTest = [
        testGroup "Meta Group" [
          testCase "first test" firstTest,
          testCase "second text" secondTest,
          testCase "third test" thirdTest,
          testCase "typeclassTest" typeclassTest,
          testCase "secondTypeclassTest" secondTypeclassTest,
          testCase "thirdTypeclassTest" thirdTypeclassTest,
          testCase "extractAllFunctionsTest" extractAllFunctionsTest,
          testCase "locationTest" locationTest,
          testCase "extensionTest" extensionTest
          ]
        ]

firstTest = "firstTest" @=? fst (head  $(functionExtractor "^first"))

secondTest = "Main" @=? $(locationModule)

thirdTest = snd (head $(functionExtractor "thirdFunction")) @=? "hej"

-- The second line wouold not work if the file is parsed by calling lex on every line.
thirdFunction = "hej" ++
  tcBlah
  where tcBlah = ""

typeclassTest =
  do let expected = "thirdFunctionhej"
         res :: [String]
         res = $(functionExtractorMap "thirdFunction" [| \n f -> n ++ f |])
         actual = head res
     expected @=? actual

secondTypeclassTest =
  do let expected = ["45", "88.8", "\"hej\""]
         actual = $(functionExtractorMap "^tc" [|\n f -> show f|] )
     expected @=? actual

thirdTypeclassTest =
  do let expected = 1
         actual = length $(functionExtractor "functionWithArgument")
     expected @=? actual

extractAllFunctionsTest =
  do let expected = 3
         actual = length $(functionExtractor "WithArgument$")
     expected @=? actual

-- Functions for extractAllFunctionsTest. They must have the same type signature.
functionWithArgument xs = "hej" ++ xs

fooWithArgument xs = "bye" ++ xs

barWithArgument xs = tcTest ++ xs
  where tcTest = "test"

-- Constructors and data statements will be ignored by function extractor.
data DataTypeWithArgument = ConstructorWithArgument Int | AnotherWithArgument String

-- Functions for testing functionExtractorMap.
tcInt :: Integer
tcInt = 45

tcDouble :: Double
tcDouble = 88.8

tcString :: String
tcString = "hej"

-- Making sure that commented code is not a problem.
{-
tcFoo :: String
tcFoo = "hej"
-}
