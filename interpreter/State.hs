module State where

import AbsLang
import Value
import qualified Data.Map as Map

data StateData = State {
  environment_stack :: [Environment],
  store :: Map.Map Location (Type, Value),
  next :: Location,
  output :: String
}

state_empty = State [Map.empty] Map.empty 0 ""

state_top_scope State { environment_stack = h:_ }  = h

state_environment State { environment_stack = e } = e

state_store State { store = store } = store

state_next State { next = next } = next

state_output State { output = output } = output

state_store_lookup location state = Map.lookup location (state_store state)

state_location_lookup ident State { environment_stack = stack, store = store } =
  foldl func Nothing stack where
    func acc env = case acc of
      Nothing -> Map.lookup ident env
      _ -> acc

state_value_lookup ident state =
  case state_location_lookup ident state of
    Just location -> state_store_lookup location state
    Nothing -> Nothing

state_add_variable ident variable state@State {
  environment_stack = top:rest, store = store, next = next
} = state {
  environment_stack = new_env:rest,
  store = new_store,
  next = next + 1
} where
  new_env = Map.insert ident next top
  new_store = Map.insert next variable store

state_set_variable location variable state@State { store = store } =
  state { store = new_store } where
    new_store = Map.insert location variable store

state_set_output output state = state { output = output }

state_add_scope scope state@State { environment_stack = e } =
  state { environment_stack = scope:e }
