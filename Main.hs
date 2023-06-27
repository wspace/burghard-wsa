module Main where

import System.Console.GetOpt
import System.Environment

import Trans

-- The header printed by wsintercpp and wsinterws, with the title modified
header =
  "Whitespace Assembler\n\
  \Made by Oliver Burghard Smarty21@gmx.net\n\
  \in his free time for your and his joy\n\
  \good time and join me to get Whitespace ready for business\n\
  \For any other information dial 1-900-WHITESPACE\n\
  \Or get soon info at www.WHITESPACE-WANTS-TO-BE-TAKEN-SERIOUS.org\n"

usage = "Usage: wsa [OPTIONS] FILENAME\n"

data Options = Options
  { optWsaOptions  :: [String]
  , optListOptions :: Bool
  , optExtSyntax   :: Bool
  , optWriteFiles  :: Bool
  , optPws         :: Bool
  , optEnded       :: Bool
  , optVersion     :: Bool
  , optHelp        :: Bool
  } deriving Show

defaultOptions = Options
  { optWsaOptions  = []
  , optListOptions = False
  , optExtSyntax   = False
  , optWriteFiles  = False
  , optPws         = False
  , optEnded       = False
  , optVersion     = False
  , optHelp        = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['o'] ["option"]
      (ReqArg (\opt opts -> opts { optWsaOptions = optWsaOptions opts ++ [opt] })
              "OPTION")
      "Enable an option in ifoption blocks"
  , Option ['l'] ["list-options"]
      (NoArg (\opts -> opts { optListOptions = True }))
      "List the options that can be used in a wsa file"
  , Option ['e'] ["ext-syntax"]
      (NoArg (\opts -> opts { optExtSyntax = True }))
      "Include debug_printstack and debug_printheap instructions"
  , Option ['w'] ["write-files"]
      (NoArg (\opts -> opts { optWriteFiles = True }))
      "Write to .ws and .pws files"
  , Option ['p'] ["pws"]
      (NoArg (\opts -> opts { optPws = True }))
      "Print with 'a' for space, 'b' for tab, and 'c' for LF"
  , Option ['q'] ["ended"]
      (NoArg (\opts -> opts { optEnded = True }))
      "Append \"\\n\\n\\nquit\\n\\n\\n\" to the program and \"\\n\\n\\n\" to the PWS program"
  , Option ['V'] ["version"]
      (NoArg (\opts -> opts { optVersion = True }))
      "Print credits"
  , Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True }))
      "Print help text"
  ]

stripLF :: String -> String
stripLF "\n" = ""
stripLF (c : s) = c : stripLF s
stripLF "" = ""

stripWsa :: String -> String
stripWsa ".wsa" = ""
stripWsa (c : s) = c : stripWsa s
stripWsa "" = ""

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
  case getOpt Permute options argv of
    (opts,args,[]) -> return (foldl (flip id) defaultOptions opts, args)
    (_,_,errs)     -> ioError (userError (stripLF (concat errs)))

withFilename :: (String -> Options -> IO ()) -> Options -> [String] -> IO ()
withFilename f opts args
  | optHelp opts = putStr (usageInfo usage options)
  | optVersion opts = putStr header
  | otherwise = case args of
      [filename] -> f filename opts
      [] -> ioError (userError "missing filename")
      _  -> ioError (userError "too many arguments")

translate :: String -> Options -> IO ()
translate filename opts =
  let filenameNoExt = stripWsa filename in do
  stringList <- readFileToStringListWithIncludes filenameNoExt
  if optListOptions opts then
    putStrLn (show (getStringListOptions stringList))
  else
    let stringListPrecompiled = precompileStringList stringList (optWsaOptions opts) in
    let ops = normalizeOps (stringListToOps stringListPrecompiled) in
    let pws = translateOpsToPWS ops (getLabels ops) (optExtSyntax opts) in
    let ws = translatePWSToWS pws in
    let pwsEnded = if optEnded opts then translatePWSToEndedPWS pws else pws in
    let wsEnded = if optEnded opts then translateWSToEndedWS ws else ws in
    if optWriteFiles opts then do
      writeFile (filenameNoExt ++ ".pws") pwsEnded
      writeFile (filenameNoExt ++ ".ws") wsEnded
    else do
      putStr (if optPws opts then pwsEnded else wsEnded)

main :: IO ()
main = do
  args <- getArgs
  (opts, args) <- parseOpts args
  withFilename translate opts args
