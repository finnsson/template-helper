{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Language.Haskell.ExtractTest where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Language.Haskell.Extract 

main = defaultMain groupsOfTest

groupsOfTest = [
        testGroup "Meta Group" [
          testCase "first test" firstTest,
          testCase "second text" secondTest,
          testCase "third test" thirdTest,
          testCase "typeclassTest" typeclassTest,
          testCase "secondTypeclassTest" secondTypeclassTest,
          testCase "thirdTypeclassTest" thirdTypeclassTest
          ]
        ]

firstTest = "firstTest" @=? fst (head  $(functionExtractor "^first"))

secondTest = "TemplateHelperTest" @=? $(locationModule)

thirdTest = snd (head $(functionExtractor "thirdFunction")) @=? "hej"

thirdFunction = "hej"




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

functionWithArgument xs = "hej" ++ xs

tcInt :: Integer
tcInt = 45

tcDouble :: Double
tcDouble = 88.8

tcString :: String
tcString = "hej"
