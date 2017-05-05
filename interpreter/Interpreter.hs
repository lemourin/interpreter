module Interpreter where

import AbsLang
import LexLang
import ParLang
import ErrM
import qualified Data.Map.Strict as Map

data ValueData = Variable {
  type_data :: String
}

data StateData = State {
  identifiers :: Map.Map String ValueData
}

interpret_expression exp state = ("dd", state)

interpret code = interpret' code (State Map.empty) where
  interpret' code state = 
    case code of
      CCode line rest -> 
        case line of
          LExpr expr -> 
            let
              (value, new_state) = interpret_expression exp state
            in
              value ++ "\n" ++ (interpret' rest new_state)
      CEmpty -> "no lines"
