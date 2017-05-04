module Main where

import AbsLang
import LexLang
import ParLang
import ErrM
import Interpreter

main = interact lang

lang s = 
  let 
    Ok a = pCode (myLexer s) 
  in 
    show (interpret a)

