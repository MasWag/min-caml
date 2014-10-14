type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  | Var of t option ref

let gentyp () = Var(ref None) (* 新しい型変数を作る *)
let rec string_of_t t =
  match t with
  | Unit -> "UNIT"
  | Bool -> "BOOL"
  | Int -> "INT"
  | Float -> "FLOAT"
  | Fun (args,t) -> "(" ^ string_of_funs (args @ [t]) ^ ")"
  | Tuple t -> "(" ^ string_of_tuple t ^ ")"
  | Array t -> "Array of " ^ (string_of_t t)
  | Var t -> match (!t) with
	     | None -> "UNDEFINED"
	     | Some t -> string_of_t t
and string_of_tuple t =
  match t with
  | [] -> ""
  | a::[] -> (string_of_t a)
  | a::d -> (string_of_t a) ^ " , " ^ (string_of_tuple d)
and string_of_funs t =
  match t with
  | [] -> ""
  | a::[] -> (string_of_t a)
  | a::d -> (string_of_t a) ^ " -> " ^ (string_of_funs d)
