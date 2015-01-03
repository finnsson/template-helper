module Language.Haskell.Extract (
  functionExtractor,
  functionExtractorMap,
  locationModule
) where
import Language.Haskell.TH
import Text.Regex.Posix
import Data.List
import System.IO

extractAllFunctions :: String -> Q [String]
extractAllFunctions pattern =
  do loc <- location
     file <- runIO $ do
       h <- openFile (loc_filename loc) ReadMode
       -- Haskell programs are usually written in UTF-8, but the default file
       -- encoding is determined by locale and may be different (e.g. CP932 encoding
       -- is used on Japanese Windows environment).
       -- To avoid "hGetContents: invalid argument (invalid byte sequence)" error
       -- on such cases, we explicitly specify file encoding as UTF-8.
       hSetEncoding h =<< mkTextEncoding "UTF-8//IGNORE"
       hGetContents h
     return $ nub $ filter (=~pattern) $ map fst $ concat $ map lex $ lines file

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
