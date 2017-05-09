module Interpreter where

import AbsLang
import LexLang
import ParLang
import PrintLang
import ErrM
import Value
import State
import qualified Data.Map as Map

interpret_function_decl :: FDecl -> StateData -> Err (Value, StateData)
interpret_function_decl (FFunctionDecl args ret code) state@State {
  environment_stack = env_stack
} =
  Ok ((ValueFunction env_stack ret arguments code), state) where
    arguments = extract_args args where
      extract_args args =
        case args of
          FNoTypedArguments -> []
          FTypedArguments args ->
            case args of
              (FOneTypedArgument (FFunctionArgClause tt ident)) -> [(tt, ident)]
              (FManyTypedArguments (FFunctionArgClause tt ident) rest) ->
                (tt, ident):(extract_args (FTypedArguments rest))

interpret_call :: Call -> StateData -> Err (Value, StateData)
interpret_call (FCall stmt call_args) state =
  case interpret_statement stmt state of
    Ok (ValueFunction env_stack ret fargs code, state) ->
      case fstate of
        Ok fstate ->
          case interpret_scope code fstate of
            Ok (ValueReturn value, new_state) -> evaluate value new_state
            Ok (value, new_state) -> evaluate value new_state
            Bad descr -> Bad descr
          where
            evaluate value new_state =
              case match_type ret value of
                True -> Ok (value, new_state { environment_stack = nenv }) where
                  nenv = state_environment state
                False -> Bad (match_type_error ret value)
        Bad descr -> Bad descr
      where
        fstate =
          case get_fstate call_args fargs (state_add_scope Map.empty state) of
            Ok (state@State { environment_stack = argenv:_ }) ->
              Ok (state { environment_stack = argenv:env_stack })
            Bad descr -> Bad descr
        get_fstate args fargs state =
          case (args, fargs) of
            (FCArguments args, (tt, ident):rest) ->
              case args of
                FOneArgument expr -> get_args expr FNoArguments
                FManyArguments expr rest -> get_args expr (FCArguments rest)
              where
                get_args expr frest =
                  case interpret_expression expr state of
                    Ok (value, nstate) ->
                      case get_fstate frest rest nstate of
                        Ok nstate ->
                          Ok (state_add_variable ident (tt, value) nstate)
                        Bad descr -> Bad descr
                    Bad descr -> Bad descr
            (FNoArguments, _:_) ->
              Bad ("Too few arguments for " ++ (printTree stmt))
            (FCArguments _, []) ->
              Bad ("Too many arguments for " ++ (printTree stmt))
            (FNoArguments, []) -> Ok state
    _ -> Bad ("Invalid function call " ++ (printTree stmt))

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
    VFunc fdecl -> interpret_function_decl fdecl state
    VCall call -> interpret_call call state

interpret_expression :: Exp -> StateData -> Err (Value, StateData)
interpret_expression expr state =
  case expr of
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
            False -> Bad (match_type_error tt value)
          Bad descr -> Bad descr
      where
        add_to_state ident value state@State { environment_stack = top:rest } =
          case Map.lookup ident (state_top_scope state) of
            Nothing -> Ok (ValueVoid, new_state) where
              new_state =
                case value of
                  ValueFunction (ftop:env) ret arg code ->
                    state {
                      environment_stack = ntop:rest,
                      store = nstore,
                      next = nnext
                    } where
                      ntop = Map.insert ident next top
                      nftop = Map.insert ident next ftop
                      nstore = Map.insert next (tt, nvalue) (state_store state)
                      nvalue = ValueFunction (nftop:env) ret arg code
                      nnext = next + 1
                      next = state_next state
                  _ -> state_add_variable ident (tt, value) state
            Just _ -> Bad ((show ident) ++ " already declared")

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
                False -> Bad (match_type_error tt value)
                True -> Ok (value, updated_state)
              where
                Just (tt, _) = state_store_lookup location new_state
                updated_state =
                  state_set_variable location (tt, value) new_state
            Bad descr -> Bad descr
    _ -> Bad ("Invalid lvalue " ++ printTree stmt)

interpret_scope :: Code -> StateData -> Err (Value, StateData)
interpret_scope code state@State { environment_stack = env } =
  case interpret code state { environment_stack = Map.empty:env } of
    Ok (value, state) -> Ok (value, state { environment_stack = env })
    Bad descr -> Bad descr

interpret_condition :: Exp -> Code -> Code -> StateData ->
  Err (Value, StateData)
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
            Ok (value, state) -> interpret_while expr code state
            Bad descr -> Bad descr
        ValueBool False -> Ok (ValueVoid, new_state)
    Bad descr -> Bad descr

interpret_print :: Exp -> StateData -> Err (Value, StateData)
interpret_print expr state =
  case interpret_expression expr state of
    Ok (value, state) -> Ok (ValueVoid, state_set_output str state) where
      str = (state_output state) ++ show' value ++ "\n"
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
        case line of
          LReturn expr ->
            case interpret_expression expr state of
              Ok (value, state) -> Ok (ValueReturn value, state)
              Bad descr -> Bad descr
          _ ->
            case interpret_line line state of
              Ok (ValueReturn value, new_state) ->
                Ok (ValueReturn value, new_state)
              Ok (value, new_state) -> interpret rest new_state
              Bad descr -> Bad descr
      CEmpty -> Ok (ValueVoid, state)
