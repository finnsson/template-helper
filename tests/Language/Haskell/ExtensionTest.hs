{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
module Language.Haskell.ExtensionTest where

import Test.HUnit

import Language.Haskell.Extract 

extensionTest = "magicConstant#" @=? fst (head $(functionExtractor "^magic"))

-- Verifying that declared language extensions do not cause parsing problems.
magicConstant# = 57
