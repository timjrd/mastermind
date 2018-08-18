module Options
  ( Option (Option)
  , Option'
  , Options
  , (!!!)
  , (...)
  , options
  , usage
  , withOptions ) where

import Data.Typeable
import Data.Proxy
import Data.Maybe
import Data.Either
import Data.List (intercalate)
import Data.Map (Map, fromList, toList, keys, union, (\\), (!))

import Text.Read (readMaybe)

import System.IO
import System.Environment

import Util

data Option a = Option  String        a      String
data Option'  = Option' String String String String (String -> Bool)

data Options  = Options (Map String String)

read' :: (Read a) => String -> Maybe a
read' str = normal ||| autoJust ||| autoNothing ||| autoTrue
  where
    normal = readMaybe str
    autoJust = readMaybe $ "Just (" ++ str ++ ")"
    autoNothing | null str  = readMaybe "Nothing"
                | otherwise = Nothing
    autoTrue | null str  = readMaybe "True"
             | otherwise = Nothing

(!!!) :: Read a => Options -> Option a -> a
(Options opts) !!! (Option name _ _) = fromJust $ read' $ opts ! name

(...) :: (Typeable a, Show a, Read a) => [Option'] -> Option a -> [Option']
xs ... (Option name value desc) =
  Option' name (show $ typeOf value) (show value) desc f : xs
  where f str = isJust $ read' str `asTypeOf` Just value

_help = Option "help" False "show this help message"

withOptions :: [Option'] -> (Options -> IO ()) -> IO ()
withOptions defaults f = do
  args <- getArgs
  cmd  <- getProgName
  
  let err = hPutStrLn stderr
      defaults' = defaults ... _help
      usage' = hPutStr stderr $ usage cmd defaults'
      
  case options defaults' args of
    (Right opts) -> if opts !!! _help then usage' else f opts
    (Left  msg ) -> err msg >> err ("try \"" ++ cmd ++ " --help\"")

options :: [Option'] -> [String] -> Either String Options
options defaults args = parse args >>= check defaults

parse :: [String] -> Either String Options
parse args = if any isLeft all
  then Left $ "option(s) " ++ showStrs (lefts all) ++ " must be preceded by two dashes"
  else Right $ Options $ fromList $ rights all
  where
    all = map parseOne args
    parseOne arg = let
      valid = take 2 arg == "--"
      clean = drop 2 arg
      key   = takeWhile (/='=') clean
      value = case dropWhile (/='=') clean of
        []     -> []
        (_:[]) -> []
        (_:v ) -> v
      in if valid
      then Right (key,value)
      else Left arg

check :: [Option'] -> Options -> Either String Options
check defaults (Options options) = if null unknown && null unparsable
  then Right $ Options $ union'
  else Left  $ intercalate "\n" $ unknownMsg ++ unparsableMsg
  where
    unknown = options \\ defaults'
    defaults' = fromList $ map (\(Option' k _ v _ _) -> (k,v)) defaults
    union' = union options defaults' \\ unknown
    isParsable = (!) $ fromList $ map (\(Option' k _ _ _ v) -> (k,v)) defaults
    unparsable = filter (not . (uncurry isParsable)) $ toList union'
    unknownMsg | null unknown = []
               | otherwise    = ["unknown option(s) " ++ showStrs (keys unknown)]
    unparsableMsg | null unparsable = []
                  | otherwise       = ["unable to parse option(s) " ++ showStrs (map fst unparsable)]

usage :: String -> [Option'] -> String
usage cmd defaults = "USAGE:\n  " ++ cmd ++ " --option1=value1 --option2=value2 ...\nOPTIONS:\n" ++ options
  where options = unlines $ header : (map line defaults)
        header                               = "  " ++ pad "OPTION"       ++ pad "TYPE" ++ pad "DEFAULT" ++ "DESCRIPTION"
        line (Option' name typ value desc _) = "  " ++ pad ("--" ++ name) ++ pad typ    ++ pad value     ++ desc
        pad str = str ++ replicate (max 1 $ 15 - length str) ' '

showStrs :: [String] -> String
showStrs xs = "\"" ++ intercalate "\", \"" xs ++ "\""
