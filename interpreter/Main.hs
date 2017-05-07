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
    case interpret a state_empty of 
      Ok (code, _) -> show' code
      Bad err -> show' (ValueString err)

