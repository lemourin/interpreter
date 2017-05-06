module Interpreter where

import AbsLang
import LexLang
import ParLang
import ErrM
import qualified Data.Map as Map

data ValueGeneric a b = ValueString a | ValueInteger b | ValueVoid
type Value = ValueGeneric String Integer
type Location = Integer

data StateData = State {
  environment_stack :: [Map.Map Ident Location],
  store :: Map.Map Location (Type, Value),
  next :: Location
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

interpret_statement :: Statement -> StateData -> Err (Value, StateData)
interpret_statement stmt state = 
  case stmt of
    VInt i -> Ok (ValueInteger i, state)
    VString i -> Ok (ValueString i, state)
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
          case state_value_lookup ident state of
            Nothing -> Ok (ValueVoid, new_state) where
              new_state = state_add_variable ident (tt, value) state
            Just _ -> Bad ((show ident) ++ " already declared")
        default_value tt = case tt of
          Tint -> ValueInteger 0
          Tstring -> ValueString ""

interpret_assignment :: Statement -> Exp -> StateData -> Err (Value, StateData)
interpret_assignment stmt expr state = 
  case stmt of 
    VIdent ident -> 
      case state_value_lookup ident state of 
        Nothing -> Bad (show ident ++ " not found")
        Just (tt, _) -> 
          case interpret_expression expr state of
            Ok (value, new_state) -> 
              case match_type tt value of 
                False -> Bad "Type mismatch"
                True -> Ok (value, updated_state)
              where
                updated_state = state_add_variable ident (tt, value) new_state
            Bad descr -> Bad descr
    _ -> Bad "Invalid lvalue"

interpret_line :: Line -> StateData -> Err (Value, StateData)
interpret_line line state = 
  case line of
    LExpr expr -> interpret_expression expr state
    LDecl decl -> interpret_declaration decl state
    LAssign stmt expr -> interpret_assignment stmt expr state

interpret :: Code -> (Err Value)
interpret code = interpret' code (State [Map.empty] Map.empty 0) where
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
