module Interpreter where

import AbsLang
import LexLang
import ParLang
import ErrM
import qualified Data.Map as Map

data ValueGeneric a b c = ValueString a       | 
                          ValueInteger b      | 
                          ValueBool c         | 
                          ValueVoid           |
                          ValueOutputString a
type Value = ValueGeneric String Integer Bool
type Location = Integer

data StateData = State {
  environment_stack :: [Map.Map Ident Location],
  store :: Map.Map Location (Type, Value),
  next :: Location
}

show' :: Value -> String
show' (ValueString str) = str
show' (ValueInteger int) = show int
show' (ValueBool bool) = show bool
show' (ValueOutputString str) = str
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
equal' (ValueInteger v1) (ValueInteger v2) = Ok (ValueBool (v1 == v2))
equal' (ValueString v1) (ValueString v2) = Ok (ValueBool (v1 == v2))
equal' (ValueBool v1) (ValueBool v2) = Ok (ValueBool (v1 == v2))
equal' _ _ = Bad "equal' :: Invalid types"
nequal' (ValueInteger v1) (ValueInteger v2) = Ok (ValueBool (v1 /= v2))
nequal' (ValueString v1) (ValueString v2) = Ok (ValueBool (v1 /= v2))
nequal' (ValueBool v1) (ValueBool v2) = Ok (ValueBool (v1 /= v2))
nequal' _ _ = Bad "nequal' :: Invalid types"
gequal' (ValueInteger v1) (ValueInteger v2) = Ok (ValueBool (v1 >= v2))
gequal' (ValueString v1) (ValueString v2) = Ok (ValueBool (v1 >= v2))
gequal' (ValueBool v1) (ValueBool v2) = Ok (ValueBool (v1 >= v2))
gequal' _ _ = Bad "gequal' :: Invalid types"
lequal' (ValueInteger v1) (ValueInteger v2) = Ok (ValueBool (v1 <= v2))
lequal' (ValueString v1) (ValueString v2) = Ok (ValueBool (v1 <= v2))
lequal' (ValueBool v1) (ValueBool v2) = Ok (ValueBool (v1 <= v2))
lequal' _ _ = Bad "lequal' :: Invalid types"
greater' (ValueInteger v1) (ValueInteger v2) = Ok (ValueBool (v1 > v2))
greater' (ValueString v1) (ValueString v2) = Ok (ValueBool (v1 > v2))
greater' (ValueBool v1) (ValueBool v2) = Ok (ValueBool (v1 > v2))
greater' _ _ = Bad "greater' :: Invalid types"
less' (ValueInteger v1) (ValueInteger v2) = Ok (ValueBool (v1 < v2))
less' (ValueString v1) (ValueString v2) = Ok (ValueBool (v1 < v2))
less' (ValueBool v1) (ValueBool v2) = Ok (ValueBool (v1 < v2))
less' _ _ = Bad "less' :: Invalid types"
or' (ValueBool v1) (ValueBool v2) = Ok (ValueBool (v1 || v2))
or' _ _ = Bad "or' :: Invalid types"
and' (ValueBool v1) (ValueBool v2) = Ok (ValueBool (v1 && v2))
and' _ _ = Bad "and' :: Invalid types"

state_empty = State [Map.empty] Map.empty 0
state_top_env (State (h:_) _ _) = h
state_store (State _ store _) = store
state_next (State _ _ next) = next
state_store_lookup location state = Map.lookup location (state_store state)
state_location_lookup ident (State stack store _) =
  foldl func Nothing stack where
    func acc env = case acc of 
      Nothing -> Map.lookup ident env
      _ -> acc
state_value_lookup ident state = 
  case state_location_lookup ident state of
    Just location -> state_store_lookup location state
    Nothing -> Nothing
state_add_variable ident variable (State (top:rest) store next) = 
  State (new_env:rest) new_store (next + 1) where
    new_env = Map.insert ident next top
    new_store = Map.insert next variable store
state_set_variable location variable (State env store next) =
  State env new_store next where
    new_store = Map.insert location variable store

concat_outputs v1 v2 =
  case (v1, v2) of 
    (ValueOutputString str1, ValueOutputString str2) ->
      ValueOutputString (str1 ++ "\n" ++ str2)
    (ValueOutputString _, _) -> v1
    (_, ValueOutputString _) -> v2
    _ -> ValueVoid
concat_descr v descr =
  case v of
    ValueOutputString str -> str ++ "\n" ++ descr
    _ -> descr

interpret_statement :: Statement -> StateData -> Err (Value, StateData)
interpret_statement stmt state = 
  case stmt of
    VInt i -> Ok (ValueInteger i, state)
    VString i -> Ok (ValueString i, state)
    VTrue -> Ok (ValueBool True, state)
    VFalse -> Ok (ValueBool False, state)
    VIdent i -> case state_value_lookup i state of
      Just (_, v) -> Ok (v, state)
      Nothing -> Bad ((show i) ++ " not found")

interpret_expression :: Exp -> StateData -> Err (Value, StateData)
interpret_expression exp state = 
  case exp of
    EAdd f1 f2 -> evaluate f1 f2 state add'
    ESub f1 f2 -> evaluate f1 f2 state sub'
    EMul f1 f2 -> evaluate f1 f2 state mul'
    EDiv f1 f2 -> evaluate f1 f2 state div'
    EEqual f1 f2 -> evaluate f1 f2 state equal'
    ENEqual f1 f2 -> evaluate f1 f2 state nequal'
    EGEqual f1 f2 -> evaluate f1 f2 state gequal'
    ELEqual f1 f2 -> evaluate f1 f2 state lequal'
    EGreater f1 f2 -> evaluate f1 f2 state greater'
    ELess f1 f2 -> evaluate f1 f2 state less'
    EOr f1 f2 -> evaluate f1 f2 state or'
    EAnd f1 f2 -> evaluate f1 f2 state and'
    EInc stmt e -> interpret_assignment stmt (EAdd (ELiteral stmt) e) state
    EDec stmt e -> interpret_assignment stmt (ESub (ELiteral stmt) e) state
    EInc1 stmt -> 
      interpret_assignment stmt (EAdd (ELiteral stmt) (ELiteral (VInt 1))) state
    EDec1 stmt ->
      interpret_assignment stmt (ESub (ELiteral stmt) (ELiteral (VInt 1))) state
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
    (Tbool, ValueBool _) -> True
    (Tstring, ValueString _) -> True
    (Tvoid, ValueVoid) -> True
    (Tauto, _) -> True
    _ -> False

interpret_declaration :: Declaration -> StateData -> Err (Value, StateData)
interpret_declaration decl state = 
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
        add_to_state ident value state = 
          case Map.lookup ident (state_top_env state) of
            Nothing -> Ok (ValueVoid, new_state) where
              new_state = state_add_variable ident (tt, value) state
            Just _ -> Bad ((show ident) ++ " already declared")
        default_value tt = case tt of
          Tint -> ValueInteger 0
          Tbool -> ValueBool False
          Tstring -> ValueString ""

interpret_assignment :: Statement -> Exp -> StateData -> Err (Value, StateData)
interpret_assignment stmt expr state = 
  case stmt of 
    VIdent ident -> 
      case state_location_lookup ident state of 
        Nothing -> Bad (show ident ++ " not found")
        Just location -> 
          case interpret_expression expr state of
            Ok (value, new_state) -> 
              case match_type tt value of 
                False -> Bad "Type mismatch"
                True -> Ok (value, updated_state)
              where
                Just (tt, _) = state_store_lookup location new_state
                updated_state = 
                  state_set_variable location (tt, value) new_state
            Bad descr -> Bad descr
    _ -> Bad "Invalid lvalue"

interpret_scope :: Code -> StateData -> Err (Value, StateData)
interpret_scope code (State (t:rest) store next) =
  case interpret code (State (Map.empty:t:rest) store next) of
    Ok (value, State _ store next) -> Ok (value, (State (t:rest) store next))
    Bad descr -> Bad descr

interpret_condition :: Exp -> Code -> Code -> StateData -> Err (Value, StateData)
interpret_condition expr code1 code2 state = 
  case interpret_expression expr state of 
    Ok (value, new_state) -> 
      case value of 
        ValueBool True -> interpret_scope code1 new_state 
        ValueBool False -> interpret_scope code2 new_state
    Bad descr -> Bad descr

interpret_while :: Exp -> Code -> StateData -> Err (Value, StateData)
interpret_while expr code state = 
  case interpret_expression expr state of
    Ok (value, new_state) ->
      case value of
        ValueBool True -> 
          case interpret_scope code new_state of 
            Ok (value, state) -> 
              case interpret_while expr code state of 
                Ok (next_value, next_state) -> 
                  Ok ((concat_outputs value next_value), next_state)
                Bad descr -> Bad (concat_descr value descr)
            Bad descr -> Bad descr
        ValueBool False -> Ok (ValueVoid, new_state)
    Bad descr -> Bad descr

interpret_print :: Exp -> StateData -> Err (Value, StateData)
interpret_print exp state =
  case interpret_expression exp state of 
    Ok (value, state) -> Ok ((ValueOutputString (show' value)), state)
    Bad descr -> Bad descr

interpret_line :: Line -> StateData -> Err (Value, StateData)
interpret_line line state = 
  case line of
    LExpr expr -> interpret_expression expr state
    LDecl decl -> interpret_declaration decl state
    LAssign stmt expr -> interpret_assignment stmt expr state
    LElse expr code1 code2 -> interpret_condition expr code1 code2 state
    LCond expr code -> interpret_condition expr code CEmpty state
    LWhile expr code -> interpret_while expr code state
    LPrint expr -> interpret_print expr state

interpret :: Code -> StateData -> Err (Value, StateData)
interpret code state = 
    case code of
      CCode line rest ->
        case interpret_line line state of 
          Ok (value, new_state) -> 
            case interpret rest new_state of
              Ok (next_value, state) -> 
                Ok (concat_outputs value next_value, state)
              Bad descr -> Bad (concat_descr value descr)
          Bad descr -> Bad descr
      CEmpty -> Ok (ValueVoid, state)
