-- automatically generated by BNF Converter
module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexM
import ParM
import SkelM
import PrintM
import AbsM
import AstM
import ErrM
import SymbolTable
import SymbolTypes
import Wff
import CodeGen

comment :: [LexM.Token] -> Int -> [LexM.Token]
comment [] 0 = []

comment (t1@(PT pos1 _):t2@(PT pos2 _):ts) n
    | str1 == "/" && str2 == "*"                = case ts of
                                                    [] -> [Err pos1]
                                                    _ -> comment ts (n+1)
    | str1 == "*" && str2 == "/" 
        && ((n <= 0) || (ts == [] && n > 0))    = [Err pos1]
    | str1 == "*" && str2 == "/"                = comment ts (n-1)
    | n > 0                                     = comment (t2:ts) n
    | n == 0                                    = t1:(comment (t2:ts) n)
    where
        str1 = prToken t1
        str2 = prToken t2

comment [t@(PT pos tok)] n
    | n > 0 || n < 0        = [Err pos]
    | otherwise             = [t]        

hasErr :: [LexM.Token] -> Bool
hasErr [] = False
hasErr (Err p:ts) = True
hasErr (t:ts) = hasErr ts

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: Verbosity -> FilePath -> IO ()
runFile v f = putStrLn f >> readFile f >>= run v 

run :: Verbosity -> String -> IO ()
run v s = let ts = myLLexer s in case (pProg ts) of
            Bad s   ->  do 
                        putStrLn $ "\nParse failed...\n"
                        putStrV v "Tokens:"
                        putStrV v $ show ts
                        putStrLn s
                        exitFailure
            Ok tree ->  do 
                        putStrLn $ "\nParse successful!\n"
                        let ast = transProg tree
                        putStrLn $ ("\n[Abstract Syntax]\n\n" ++ (show ast))
                        putStrV v $ ("\n[Linearized tree]\n\n" ++ printTree tree)
                        let sfiprog = wffProg ast
                        if (isSS sfiprog)
                            then do 
                                putStrLn $ "\nSemantic analysis successful!\n"
                                putStrLn $ "\n[Intermediate representation]\n\n"
                                putStrLn $ show (fromSS sfiprog)
				let iprogCode = genIProg (fromSS sfiprog)
				handle <- openFile "machine_code" WriteMode
				hPutStrLn handle iprogCode
			    	hClose handle
                            else do
                                putStrLn $ "\nFailure in semantic analysis...\n"
                                putStrLn $ fromFF sfiprog
                        exitSuccess



usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> hGetContents stdin >>= run 2
    "-s":fs -> mapM_ (runFile 0) fs
    fs -> mapM_ (runFile 2) fs


                          
