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
  locationModule
) where
import Language.Haskell.TH
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Text.Regex.Posix
import Maybe
import Language.Haskell.Exts.Extension

extractAllFunctions pattern  = 
  allMatchingFunctions pattern . parsedModule

parsedModule moduleCode = 
  let pMod = parseModuleWithMode ( ParseMode "test" [TemplateHaskell] False [] ) moduleCode
      moduleOrDefault (ParseFailed _ _) = Module (SrcLoc "unknown" 1 1) (ModuleName "unknown") [] Nothing Nothing [] []
      moduleOrDefault (ParseOk m) = m
  in moduleOrDefault pMod 

allFunctions = onlyJust hsIdent . onlyJust hsPVar . onlyJust hsPatBind . hsModuleDecls 
allMatchingFunctions pattern = filter (\f->f=~pattern::Bool) . allFunctions 

onlyJust f = map fromJust . filter isJust . map f

hsModuleDecls (Module _ _ _ _ _ _ d) = d
hsPatBind (PatBind _ p _ _ _) = Just p
hsPatBind _ = Nothing

hsPVar (PVar n) = Just n
hsPVar _ = Nothing

hsIdent (Ident n) = Just n
hsIdent _ = Nothing


functionExtractor :: String -> ExpQ
functionExtractor pattern =
  do loc <- location
     moduleCode <- runIO $ readFile $ loc_filename loc
     let functions = extractAllFunctions pattern moduleCode
         makePair n = TupE [ LitE $ StringL n , VarE $ mkName n]
     return $ ListE $ map makePair functions

locationModule :: ExpQ
locationModule =
  do loc <- location
     return $ LitE $ StringL $ loc_module loc
