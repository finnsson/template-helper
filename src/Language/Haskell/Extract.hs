module Language.Haskell.Extract (
  functionExtractor,
  functionExtractorMap,
  locationModule
) where

import Data.List
import Data.Maybe
import Language.Haskell.Exts.Annotated(
    Decl(..), Extension(..), Match(..), Module(..), Name(..), Pat(..),
    SrcSpanInfo, KnownExtension(..), fromParseResult, parseFileWithExts)
import Language.Haskell.TH
import Text.Regex.Posix ((=~))

defaultExtensions :: [Extension]
defaultExtensions = [EnableExtension TemplateHaskell]

extractAllFunctions :: String -> Q [String]
extractAllFunctions pattern =
  do loc <- location
     parseResult <- runIO $ parseFileWithExts defaultExtensions $ loc_filename loc
     return $ nub $ filter (=~pattern) $ mapMaybe unwrapDeclNames
         $ getDecls $ fromParseResult parseResult
  where
  getDecls (Module _ _ _ _ decls) = decls
  getDecls _ = []
  unwrapDeclNames :: Decl SrcSpanInfo -> Maybe String
  unwrapDeclNames (FunBind _ []) = Nothing
  unwrapDeclNames (FunBind _ (m:_)) = Just (fromMatch m)
  unwrapDeclNames (PatBind _ (PVar _ name) _ _) = Just $ fromName name
  unwrapDeclNames _ = Nothing
  fromName (Ident _ name) = name
  fromName (Symbol _ name) = name
  fromMatch (Language.Haskell.Exts.Annotated.Match _ name _ _ _) = fromName name
  fromMatch (Language.Haskell.Exts.Annotated.InfixMatch _ _ name _ _ _) = fromName name


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
  do functions <- extractAllFunctions pattern
     let makePair n = TupE [ LitE $ StringL n , VarE $ mkName n]
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
  do functions <- extractAllFunctions pattern
     fn <- funcName
     let makePair n = AppE (AppE (fn) (LitE $ StringL n)) (VarE $ mkName n)
     return $ ListE $ map makePair functions


-- | Extract the name of the current module.
locationModule :: ExpQ
locationModule =
  do loc <- location
     return $ LitE $ StringL $ loc_module loc
