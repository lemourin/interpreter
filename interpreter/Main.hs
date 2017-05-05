module Main where

import AbsLang
import LexLang
import ParLang
import ErrM
import Interpreter

main = do
  interact lang
  putStrLn ""

lang s = 
  let 
    Ok a = pCode (myLexer s) 
  in 
    show (interpret a)

