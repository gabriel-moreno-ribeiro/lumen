(* Abstract syntax of Lumen, a small dynamically typed language. *)

type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Le | Gt | Ge
  | And | Or

type unop = Neg | Not

type expr = { e : expr_desc; line : int }

and expr_desc =
  | Num of float
  | Str of string
  | Bool of bool
  | Nil
  | Var of string
  | List of expr list
  | Index of expr * expr
  | Binary of binop * expr * expr
  | Unary of unop * expr
  | Call of expr * expr list
  | Lambda of string list * stmt list
  | Assign of string * expr
  | IndexAssign of expr * expr * expr

and stmt = { s : stmt_desc; sline : int }

and stmt_desc =
  | Let of string * expr
  | ExprStmt of expr
  | Print of expr
  | Block of stmt list
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | For of stmt option * expr * expr option * stmt   (* init, condition, step, body *)
  | Fn of string * string list * stmt list
  | Return of expr option
  | Break
  | Continue

let binop_name = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
  | And -> "and" | Or -> "or"
