{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.LocationTest where

import Test.HUnit

import Language.Haskell.Extract 

locationTest = "Language.Haskell.LocationTest" @=? $(locationModule)
