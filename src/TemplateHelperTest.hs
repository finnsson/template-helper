-----------------------------------------------------------------------------
--
-- Module      :  TemplateHelperTest
-- Copyright   :  
-- License     :  BSD4
--
-- Maintainer  :  Oscar Finnsson
-- Stability   :  
-- Portability :  
--
-- |
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module TemplateHelperTest where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import TemplateHelper

main = defaultMain groupsOfTest

groupsOfTest = [
        testGroup "Meta Group" [
          testCase "first test" firstTest,
          testCase "second text" secondTest,
          testCase "third test" thirdTest
          ]
        ]

firstTest = "firstTest" @=? fst (head  $(functionExtractor "^first"))

secondTest = "TemplateHelperTest" @=? $(locationModule)

thirdTest = snd (head $(functionExtractor "thirdFunction")) @=? "hej"

thirdFunction = "hej"
