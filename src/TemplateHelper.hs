-----------------------------------------------------------------------------
--
-- Module      :  TemplateHelper
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

module TemplateHelper (
  functionExtractor,
  functionExtractorMap,
  locationModule
) where
import Language.Haskell.TH
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Text.Regex.Posix
import Maybe
import Language.Haskell.Exts.Extension

extractAllFunctions :: String -> String-> [String]
extractAllFunctions pattern  = 
  allMatchingFunctions pattern . parsedModule

parsedModule moduleCode = 
  let pMod = parseModuleWithMode ( ParseMode "test" [TemplateHaskell] False False [] ) moduleCode
      moduleOrDefault (ParseFailed _ _) = Module (SrcLoc "unknown" 1 1) (ModuleName "unknown") [] Nothing Nothing [] []
      moduleOrDefault (ParseOk m) = m
  in moduleOrDefault pMod 

allFunctions =  onlyJust extractNameOfFunctionFromDecl . hsModuleDecls 
allMatchingFunctions pattern = filter (\f->f=~pattern::Bool) . allFunctions 



extractNameOfFunctionFromDecl :: Decl -> Maybe String
extractNameOfFunctionFromDecl (PatBind _ (PVar (Ident n)) _ _ _ ) = Just n
extractNameOfFunctionFromDecl (FunBind ms) = Just $ head $ [n | (Language.Haskell.Exts.Syntax.Match _ (Ident n) _ _ _ _)  <- ms]
extractNameOfFunctionFromDecl _ = Nothing

onlyJust f = map fromJust . filter isJust . map f

hsModuleDecls (Module _ _ _ _ _ _ d) = d

-- | Extract the names and functions from the module where this function is called.
-- 
--  > foo = "test"
--  > boo = "testing"
--  > bar = $(functionExtractor "oo$")
-- 
-- will automagically extract the functions ending with "oo" such as
-- 
-- > bar = [("foo",foo), ("boo",boo)]
functionExtractor :: String -> ExpQ
functionExtractor pattern =
  do loc <- location
     moduleCode <- runIO $ readFile $ loc_filename loc
     let functions = extractAllFunctions pattern moduleCode
         makePair n = TupE [ LitE $ StringL n , VarE $ mkName n]
     return $ ListE $ map makePair functions


-- | Extract the names and functions from the module and apply a function to every pair.
-- 
-- Is very useful if the common denominator of the functions is just a type class.
--
-- > secondTypeclassTest =
-- >   do let expected = ["45", "88.8", "\"hej\""]
-- >          actual = $(functionExtractorMap "^tc" [|\n f -> show f|] )
-- >      expected @=? actual
-- > 
-- > tcInt :: Integer
-- > tcInt = 45
-- > 
-- > tcDouble :: Double
-- > tcDouble = 88.8
-- > 
-- > tcString :: String
-- > tcString = "hej"
functionExtractorMap :: String -> ExpQ -> ExpQ
functionExtractorMap pattern funcName =
  do loc <- location
     moduleCode <- runIO $ readFile $ loc_filename loc
     let functions :: [String]
         functions = extractAllFunctions pattern moduleCode
     fn <- funcName
     let makePair n = AppE (AppE (fn) (LitE $ StringL n)) (VarE $ mkName n)
     return $ ListE $ map makePair functions 

-- | Extract the name of the current module.
locationModule :: ExpQ
locationModule =
  do loc <- location
     return $ LitE $ StringL $ loc_module loc
