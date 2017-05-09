module Main where

import AbsLang
import LexLang
import ParLang
import ErrM
import Interpreter
import State

main = interact lang

lang s = 
  let 
    Ok a = pCode (myLexer s) 
  in 
    case interpret a state_empty of 
      Ok (_, state) -> state_output state
      Bad err -> err ++ "\n"

