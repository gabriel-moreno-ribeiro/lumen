(* Tree-walking interpreter: values, lexical environments, closures and
   built-in functions. Control flow (return / break / continue) is
   implemented with OCaml exceptions. *)

open Ast

type value =
  | VNum of float
  | VStr of string
  | VBool of bool
  | VNil
  | VList of value array ref
  | VFun of closure
  | VBuiltin of string * (int -> value list -> value)

and closure = { name : string; params : string list; body : stmt list; env : env }

and env = { vars : (string, value ref) Hashtbl.t; parent : env option }

exception Runtime_error of string * int
exception Return_exc of value
exception Break_exc
exception Continue_exc

let error line fmt = Printf.ksprintf (fun msg -> raise (Runtime_error (msg, line))) fmt

(* ---- environments ------------------------------------------------------- *)

let new_env parent = { vars = Hashtbl.create 8; parent }

let define env name v = Hashtbl.replace env.vars name (ref v)

let rec lookup env name line =
  match Hashtbl.find_opt env.vars name with
  | Some r -> r
  | None -> (
      match env.parent with
      | Some p -> lookup p name line
      | None -> error line "undefined variable '%s'" name)

(* ---- printing ----------------------------------------------------------- *)

let rec to_string = function
  | VNum f -> if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.0f" f else Printf.sprintf "%g" f
  | VStr s -> s
  | VBool b -> string_of_bool b
  | VNil -> "nil"
  | VList items -> "[" ^ String.concat ", " (Array.to_list (Array.map repr !items)) ^ "]"
  | VFun c -> "<fn " ^ c.name ^ ">"
  | VBuiltin (name, _) -> "<builtin " ^ name ^ ">"

and repr = function VStr s -> Printf.sprintf "%S" s | v -> to_string v

let type_name = function
  | VNum _ -> "number" | VStr _ -> "string" | VBool _ -> "bool" | VNil -> "nil"
  | VList _ -> "list" | VFun _ | VBuiltin _ -> "function"

let truthy = function VNil | VBool false -> false | _ -> true

let rec equal a b =
  match (a, b) with
  | VNum x, VNum y -> x = y
  | VStr x, VStr y -> String.equal x y
  | VBool x, VBool y -> x = y
  | VNil, VNil -> true
  | VList x, VList y ->
    Array.length !x = Array.length !y && Array.for_all2 equal !x !y
  | VFun x, VFun y -> x == y
  | _ -> false

(* ---- built-ins ---------------------------------------------------------- *)

let builtins (output : string -> unit) =
  let num line = function VNum f -> f | v -> error line "expected a number, got %s" (type_name v) in
  let list line = function VList l -> l | v -> error line "expected a list, got %s" (type_name v) in
  [
    ("len", fun line -> function
       | [ VStr s ] -> VNum (float_of_int (String.length s))
       | [ VList l ] -> VNum (float_of_int (Array.length !l))
       | [ v ] -> error line "len() takes a string or list, got %s" (type_name v)
       | _ -> error line "len() takes one argument");
    ("push", fun line -> function
       | [ l; v ] -> let r = list line l in r := Array.append !r [| v |]; VNil
       | _ -> error line "push() takes a list and a value");
    ("pop", fun line -> function
       | [ l ] ->
         let r = list line l in
         let n = Array.length !r in
         if n = 0 then error line "pop() from an empty list";
         let last = !r.(n - 1) in
         r := Array.sub !r 0 (n - 1);
         last
       | _ -> error line "pop() takes a list");
    ("str", fun _ -> function [ v ] -> VStr (to_string v) | _ -> VStr "");
    ("num", fun line -> function
       | [ VStr s ] -> (match float_of_string_opt (String.trim s) with Some f -> VNum f | None -> VNil)
       | [ VNum f ] -> VNum f
       | [ v ] -> error line "num() cannot convert %s" (type_name v)
       | _ -> error line "num() takes one argument");
    ("type", fun _ -> function [ v ] -> VStr (type_name v) | _ -> VNil);
    ("sqrt", fun line -> function [ v ] -> VNum (sqrt (num line v)) | _ -> error line "sqrt() takes one number");
    ("floor", fun line -> function [ v ] -> VNum (Float.of_int (int_of_float (floor (num line v)))) | _ -> error line "floor() takes one number");
    ("range", fun line -> function
       | [ a ] -> VList (ref (Array.init (max 0 (int_of_float (num line a))) (fun i -> VNum (float_of_int i))))
       | [ a; b ] ->
         let lo = int_of_float (num line a) and hi = int_of_float (num line b) in
         VList (ref (Array.init (max 0 (hi - lo)) (fun i -> VNum (float_of_int (lo + i)))))
       | _ -> error line "range() takes one or two numbers");
    ("join", fun line -> function
       | [ l; VStr sep ] -> VStr (String.concat sep (Array.to_list (Array.map to_string !(list line l))))
       | _ -> error line "join() takes a list and a separator");
    ("split", fun line -> function
       | [ VStr s; VStr sep ] when sep <> "" ->
         let parts = Str_split.split s sep in
         VList (ref (Array.of_list (List.map (fun p -> VStr p) parts)))
       | _ -> error line "split() takes a string and a non-empty separator");
    ("write", fun _ args -> output (String.concat "" (List.map to_string args)); VNil);
    ("clock", fun _ _ -> VNum (Unix_time.now ()));
  ]

(* ---- evaluation --------------------------------------------------------- *)

let rec eval env (e : expr) : value =
  match e.e with
  | Num f -> VNum f
  | Str s -> VStr s
  | Bool b -> VBool b
  | Nil -> VNil
  | Var name -> !(lookup env name e.line)
  | List items -> VList (ref (Array.of_list (List.map (eval env) items)))
  | Lambda (params, body) -> VFun { name = "lambda"; params; body; env }
  | Assign (name, value) ->
    let v = eval env value in
    lookup env name e.line := v;
    v
  | Index (obj, idx) -> (
      match (eval env obj, eval env idx) with
      | VList l, VNum i ->
        let n = int_of_float i in
        if n < 0 || n >= Array.length !l then error e.line "index %d out of range (length %d)" n (Array.length !l);
        !l.(n)
      | VStr s, VNum i ->
        let n = int_of_float i in
        if n < 0 || n >= String.length s then error e.line "index %d out of range (length %d)" n (String.length s);
        VStr (String.make 1 s.[n])
      | v, _ -> error e.line "cannot index a %s" (type_name v))
  | IndexAssign (obj, idx, value) -> (
      match (eval env obj, eval env idx) with
      | VList l, VNum i ->
        let n = int_of_float i in
        if n < 0 || n >= Array.length !l then error e.line "index %d out of range (length %d)" n (Array.length !l);
        let v = eval env value in
        !l.(n) <- v;
        v
      | v, _ -> error e.line "cannot assign into a %s" (type_name v))
  | Unary (Neg, x) -> (
      match eval env x with VNum f -> VNum (-.f) | v -> error e.line "cannot negate a %s" (type_name v))
  | Unary (Not, x) -> VBool (not (truthy (eval env x)))
  | Binary (And, a, b) -> let l = eval env a in if truthy l then eval env b else l
  | Binary (Or, a, b) -> let l = eval env a in if truthy l then l else eval env b
  | Binary (op, a, b) -> binary e.line op (eval env a) (eval env b)
  | Call (callee, args) ->
    let f = eval env callee in
    let argv = List.map (eval env) args in
    call e.line f argv

and binary line op l r =
  match (op, l, r) with
  | Add, VNum a, VNum b -> VNum (a +. b)
  | Add, VStr a, VStr b -> VStr (a ^ b)
  | Add, VStr a, b -> VStr (a ^ to_string b)
  | Add, a, VStr b -> VStr (to_string a ^ b)
  | Add, VList a, VList b -> VList (ref (Array.append !a !b))
  | Sub, VNum a, VNum b -> VNum (a -. b)
  | Mul, VNum a, VNum b -> VNum (a *. b)
  | Mul, VStr s, VNum n -> VStr (String.concat "" (List.init (max 0 (int_of_float n)) (fun _ -> s)))
  | Div, VNum _, VNum 0. -> error line "division by zero"
  | Div, VNum a, VNum b -> VNum (a /. b)
  | Mod, VNum _, VNum 0. -> error line "division by zero"
  | Mod, VNum a, VNum b -> VNum (Float.rem a b)
  | Eq, a, b -> VBool (equal a b)
  | Neq, a, b -> VBool (not (equal a b))
  | Lt, VNum a, VNum b -> VBool (a < b)
  | Le, VNum a, VNum b -> VBool (a <= b)
  | Gt, VNum a, VNum b -> VBool (a > b)
  | Ge, VNum a, VNum b -> VBool (a >= b)
  | Lt, VStr a, VStr b -> VBool (String.compare a b < 0)
  | Le, VStr a, VStr b -> VBool (String.compare a b <= 0)
  | Gt, VStr a, VStr b -> VBool (String.compare a b > 0)
  | Ge, VStr a, VStr b -> VBool (String.compare a b >= 0)
  | _ -> error line "unsupported operands for '%s': %s and %s" (binop_name op) (type_name l) (type_name r)

and call line f argv =
  match f with
  | VBuiltin (_, impl) -> impl line argv
  | VFun c ->
    if List.length argv <> List.length c.params then
      error line "%s expects %d argument(s) but got %d" c.name (List.length c.params) (List.length argv);
    let env = new_env (Some c.env) in
    List.iter2 (define env) c.params argv;
    (try exec_block env c.body; VNil with Return_exc v -> v)
  | v -> error line "cannot call a %s" (type_name v)

and exec env (s : stmt) : unit =
  match s.s with
  | Let (name, e) -> define env name (eval env e)
  | ExprStmt e -> ignore (eval env e)
  | Print e -> !(lookup env "__print" s.sline) |> fun p -> ignore (call s.sline p [ eval env e ])
  | Block body -> exec_block (new_env (Some env)) body
  | If (cond, t, f) -> if truthy (eval env cond) then exec env t else Option.iter (exec env) f
  | While (cond, body) ->
    (try
       while truthy (eval env cond) do
         try exec env body with Continue_exc -> ()
       done
     with Break_exc -> ())
  | For (init, cond, step, body) ->
    (* the init variable lives in its own scope; `continue` still runs the step *)
    let env = new_env (Some env) in
    Option.iter (exec env) init;
    (try
       while truthy (eval env cond) do
         (try exec env body with Continue_exc -> ());
         Option.iter (fun e -> ignore (eval env e)) step
       done
     with Break_exc -> ())
  | Fn (name, params, body) ->
    (* the closure captures the environment it is defined in, which includes itself: recursion works *)
    define env name (VFun { name; params; body; env })
  | Return e -> raise (Return_exc (match e with Some e -> eval env e | None -> VNil))
  | Break -> raise Break_exc
  | Continue -> raise Continue_exc

and exec_block env stmts = List.iter (exec env) stmts

(* ---- entry points ------------------------------------------------------- *)

let make_globals (output : string -> unit) : env =
  let env = new_env None in
  List.iter (fun (name, impl) -> define env name (VBuiltin (name, impl))) (builtins output);
  define env "__print" (VBuiltin ("print", fun _ args -> output (String.concat " " (List.map to_string args) ^ "\n"); VNil));
  env

(* Runs a program, returning captured output or the error message. *)
let run ?(env : env option) (src : string) (output : string -> unit) : (unit, string) result =
  let env = match env with Some e -> e | None -> make_globals output in
  try
    exec_block env (Parser.parse src);
    Ok ()
  with
  | Lexer.Lex_error (msg, line) -> Error (Printf.sprintf "syntax error on line %d: %s" line msg)
  | Parser.Parse_error (msg, line) -> Error (Printf.sprintf "syntax error on line %d: %s" line msg)
  | Runtime_error (msg, line) -> Error (Printf.sprintf "runtime error on line %d: %s" line msg)
  | Return_exc _ -> Error "runtime error: 'return' outside of a function"
  | Break_exc -> Error "runtime error: 'break' outside of a loop"
  | Continue_exc -> Error "runtime error: 'continue' outside of a loop"
  | Stack_overflow -> Error "runtime error: stack overflow (too much recursion)"
