module Value where

import AbsLang
import ErrM
import qualified Data.Map as Map

data Value =
  ValueString String   |
  ValueInteger Integer |
  ValueBool Bool       |
  ValueVoid            |
  ValueFunction [Environment] Type [(Type, Ident)] Code |
  ValueReturn Value

type Environment = Map.Map Ident Location
type Location = Integer

show' (ValueString str) = str
show' (ValueInteger int) = show int
show' (ValueBool bool) = show bool
show' ValueVoid = ""

show_type Tint = "int"
show_type Tbool = "bool"
show_type Tstring = "string"
show_type Tauto = "auto"

add' (ValueInteger v1) (ValueInteger v2) = Ok (ValueInteger (v1 + v2))
add' (ValueString v1) (ValueString v2) = Ok (ValueString (v1 ++ v2))
add' _ _ = Bad "add' :: Invalid types"

sub' (ValueInteger v1) (ValueInteger v2) = Ok (ValueInteger (v1 - v2))
sub' _ _ = Bad "sub' :: Invalid types"

mul' (ValueInteger v1) (ValueInteger v2) = Ok (ValueInteger (v1 * v2))
mul' _ _ = Bad "mul' :: Invalid types"

div' (ValueInteger v1) (ValueInteger v2) =
  case v2 of
    0 -> Bad "div' :: Division by zero"
    _ -> Ok (ValueInteger (v1 `div` v2))
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

default_value :: Type -> Value
default_value tt = case tt of
  Tint -> ValueInteger 0
  Tbool -> ValueBool False
  Tstring -> ValueString ""
  _ -> ValueVoid

match_type :: Type -> Value -> Bool
match_type tt value =
  case (tt, value) of
    (Tint, ValueInteger _) -> True
    (Tbool, ValueBool _) -> True
    (Tstring, ValueString _) -> True
    (Tvoid, ValueVoid) -> True
    (Tauto, _) -> True
    _ -> False

get_type :: Value -> Type
get_type value =
  case value of
    ValueInteger _ -> Tint
    ValueBool _ -> Tbool
    ValueString _ -> Tstring
    ValueVoid -> Tvoid
    _ -> Tauto

match_type_error :: Type -> Value -> String
match_type_error tt value =
  "Expected type " ++ show_type tt ++ ", got " ++ show_type (get_type value)
