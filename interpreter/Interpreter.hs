module Interpreter where

import AbsLang
import LexLang
import ParLang
import ErrM
import qualified Data.Map.Strict as Map

data ValueGeneric a b = ValueString a | ValueInteger b | ValueVoid
type Value = ValueGeneric String Integer

data StateData = State {
  environment_stack :: [Map.Map Ident (Type, Value)]
}

show' :: Value -> String
show' (ValueString str) = str
show' (ValueInteger int) = show int
show' ValueVoid = ""

add' (ValueInteger v1) (ValueInteger v2) = Ok (ValueInteger (v1 + v2))
add' (ValueString v1) (ValueString v2) = Ok (ValueString (v1 ++ v2))
add' _ _ = Bad "add' :: Invalid types"
sub' (ValueInteger v1) (ValueInteger v2) = Ok (ValueInteger (v1 - v2))
sub' _ _ = Bad "sub' :: Invalid types"
mul' (ValueInteger v1) (ValueInteger v2) = Ok (ValueInteger (v1 * v2))
mul' _ _ = Bad "mul' :: Invalid types"
div' (ValueInteger v1) (ValueInteger v2) = Ok (ValueInteger (v1 `div` v2))
div' _ _ = Bad "div' :: Invalid types"

top_env (State (h:t)) = h

interpret_statement :: Statement -> StateData-> Err (Value, StateData)
interpret_statement stmt state = 
  case stmt of
    VInt i -> Ok (ValueInteger i, state)
    VString i -> Ok (ValueString i, state)
    VIdent i -> case Map.lookup i (top_env state) of
      Just (tt, v) -> Ok (v, state)
      Nothing -> Bad ((show i) ++ " not found")

interpret_expression :: Exp -> StateData -> Err (Value, StateData)
interpret_expression exp state = 
  case exp of
    EAdd f1 f2 -> evaluate f1 f2 state add'
    ESub f1 f2 -> evaluate f1 f2 state sub'
    EMul f1 f2 -> evaluate f1 f2 state mul'
    EDiv f1 f2 -> evaluate f1 f2 state div'
    ELiteral e -> interpret_statement e state
  where 
    evaluate f1 f2 state func = 
      case interpret_expression f1 state of 
        Ok (value1, new_state1) -> 
          case interpret_expression f2 new_state1 of 
            Ok (value2, new_state2) -> 
              case func value1 value2 of 
                Ok value -> Ok (value, new_state2)
                Bad descr -> Bad descr
            Bad descr -> Bad descr
        Bad descr -> Bad descr

match_type :: Type -> Value -> Bool
match_type tt value = 
  case (tt, value) of
    (Tint, ValueInteger _) -> True
    (Tstring, ValueString _) -> True
    (Tvoid, ValueVoid) -> True
    _ -> False

interpret_declaration :: Declaration -> StateData -> Err (Value, StateData)
interpret_declaration decl (State (t:rest)) = 
  case decl of 
    EDecl tt declaration -> case declaration of 
      EDeclOne e -> add_variable tt e
      EDeclMany e rest ->
        case add_variable tt e of
          Ok (_, new_state) -> interpret_declaration (EDecl tt rest) new_state
          Bad descr -> Bad descr
  where
    add_variable tt decl = 
      case decl of 
        EDeclNoInit ident -> 
          add_to_state ident (default_value tt) state
        EDeclInit ident exp -> case interpret_expression exp state of
          Ok (value, new_state) -> case match_type tt value of 
            True -> add_to_state ident value new_state
            False -> Bad "Type mismatch"
          Bad descr -> Bad descr
      where
        state = (State (t:rest))
        add_to_state ident value (State (t:rest)) = 
          case Map.lookup ident t of
            Nothing -> Ok (ValueVoid, (State (new_env:rest))) where
              new_env = Map.insert ident (tt, value) t
            Just _ -> Bad ((show ident) ++ " already declared")
        default_value tt = case tt of
          Tint -> ValueInteger 0
          Tstring -> ValueString ""

interpret_assignment :: Statement -> Exp -> StateData -> Err (Value, StateData)
interpret_assignment stmt expr state = 
  case stmt of 
    VIdent ident -> 
      case Map.lookup ident (top_env state) of 
        Nothing -> Bad (show ident ++ " not found")
        Just (tt, _) -> 
          case interpret_expression expr state of
            Ok (value, new_state) -> 
              case match_type tt value of 
                False -> Bad "Type mismatch"
                True -> Ok (value, updated_state)
              where
                updated_state = case new_state of 
                  State (e:rest) -> 
                    State ((Map.insert ident (tt, value) e):rest)
            Bad descr -> Bad descr
    _ -> Bad "Invalid lvalue"

interpret_line :: Line -> StateData -> Err (Value, StateData)
interpret_line line state = 
  case line of
    LExpr expr -> interpret_expression expr state
    LDecl decl -> interpret_declaration decl state
    LAssign stmt expr -> interpret_assignment stmt expr state

interpret :: Code -> (Err Value)
interpret code = interpret' code (State [Map.empty]) where
  interpret' code state = 
    case code of
      CCode line rest ->
        case interpret_line line state of 
          Ok (value, new_state) -> 
            case interpret' rest new_state of
              Ok next_value -> Ok (ValueString str) where
                separator = case (value, next_value) of
                  (ValueVoid, _) -> ""
                  (_, ValueVoid) -> ""
                  _ -> "\n"
                str = show' value ++ separator ++ show' next_value
              Bad descr -> Bad (show' value ++ separator ++ descr) where
                separator = case value of 
                  ValueVoid -> ""
                  _ -> "\n"
          Bad descr -> Bad descr
      CEmpty -> Ok ValueVoid
