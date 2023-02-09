(* Recursive-descent parser with precedence climbing for expressions.

   program    := statement* EOF
   statement  := "let" IDENT "=" expr ";" | "fn" IDENT "(" params ")" block
               | "if" "(" expr ")" statement ("else" statement)?
               | "while" "(" expr ")" statement
               | "for" "(" (let | exprstmt | ";") expr? ";" expr? ")" statement
               | "return" expr? ";" | "print" expr ";" | "break" ";" | "continue" ";"
               | block | expr ";"
   expr       := assignment
   assignment := IDENT "=" assignment | postfix "[" expr "]" "=" assignment | or
   or         := and ("or" and)*          and := equality ("and" equality)*
   equality   := comparison (("==" | "!=") comparison)*
   comparison := term (("<" | "<=" | ">" | ">=") term)*
   term       := factor (("+" | "-") factor)*    factor := unary (("*" | "/" | "%") unary)*
   unary      := ("-" | "not") unary | postfix
   postfix    := primary ( "(" args ")" | "[" expr "]" )*
   primary    := NUMBER | STRING | true | false | nil | IDENT | "(" expr ")"
               | "[" elements "]" | "fn" "(" params ")" block
*)

open Ast
open Lexer

exception Parse_error of string * int

type state = { mutable toks : located list }

let peek st = match st.toks with t :: _ -> t | [] -> { tok = EOF; line = 0 }
let advance st = match st.toks with t :: rest -> st.toks <- rest; t | [] -> { tok = EOF; line = 0 }
let line st = (peek st).line
let fail st msg = raise (Parse_error (msg, line st))

let check st tok = (peek st).tok = tok
let accept st tok = if check st tok then (ignore (advance st); true) else false

let expect st tok what =
  if not (accept st tok) then
    fail st (Printf.sprintf "expected %s but found %s" what (token_to_string (peek st).tok))

let expect_ident st =
  match (peek st).tok with
  | IDENT name -> ignore (advance st); name
  | t -> fail st ("expected a name but found " ^ token_to_string t)

let mk_expr st e = { e; line = line st }

let rec program st =
  let rec loop acc =
    if check st EOF then List.rev acc else loop (statement st :: acc)
  in
  loop []

and block st =
  expect st (SYMBOL "{") "'{'";
  let rec loop acc =
    if accept st (SYMBOL "}") then List.rev acc
    else if check st EOF then fail st "unterminated block"
    else loop (statement st :: acc)
  in
  loop []

and params st =
  expect st (SYMBOL "(") "'('";
  if accept st (SYMBOL ")") then []
  else begin
    let rec loop acc =
      let name = expect_ident st in
      if accept st (SYMBOL ",") then loop (name :: acc)
      else (expect st (SYMBOL ")") "')'"; List.rev (name :: acc))
    in
    loop []
  end

and statement st : stmt =
  let sline = line st in
  let desc =
    match (peek st).tok with
    | KEYWORD "let" ->
      ignore (advance st);
      let name = expect_ident st in
      expect st (SYMBOL "=") "'='";
      let value = expression st in
      expect st (SYMBOL ";") "';'";
      Let (name, value)
    | KEYWORD "fn" when (match st.toks with _ :: { tok = IDENT _; _ } :: _ -> true | _ -> false) ->
      ignore (advance st);
      let name = expect_ident st in
      let ps = params st in
      let body = block st in
      Fn (name, ps, body)
    | KEYWORD "if" ->
      ignore (advance st);
      expect st (SYMBOL "(") "'('";
      let cond = expression st in
      expect st (SYMBOL ")") "')'";
      let then_branch = statement st in
      let else_branch = if accept st (KEYWORD "else") then Some (statement st) else None in
      If (cond, then_branch, else_branch)
    | KEYWORD "while" ->
      ignore (advance st);
      expect st (SYMBOL "(") "'('";
      let cond = expression st in
      expect st (SYMBOL ")") "')'";
      While (cond, statement st)
    | KEYWORD "for" ->
      ignore (advance st);
      expect st (SYMBOL "(") "'('";
      let init =
        if accept st (SYMBOL ";") then None
        else if check st (KEYWORD "let") then Some (statement st)
        else begin
          let e = expression st in
          expect st (SYMBOL ";") "';'";
          Some { s = ExprStmt e; sline }
        end
      in
      let cond = if check st (SYMBOL ";") then mk_expr st (Bool true) else expression st in
      expect st (SYMBOL ";") "';'";
      let step = if check st (SYMBOL ")") then None else Some (expression st) in
      expect st (SYMBOL ")") "')'";
      let body = statement st in
      For (init, cond, step, body)
    | KEYWORD "return" ->
      ignore (advance st);
      let value = if check st (SYMBOL ";") then None else Some (expression st) in
      expect st (SYMBOL ";") "';'";
      Return value
    | KEYWORD "print" ->
      ignore (advance st);
      let e = expression st in
      expect st (SYMBOL ";") "';'";
      Print e
    | KEYWORD "break" -> ignore (advance st); expect st (SYMBOL ";") "';'"; Break
    | KEYWORD "continue" -> ignore (advance st); expect st (SYMBOL ";") "';'"; Continue
    | SYMBOL "{" -> Block (block st)
    | _ ->
      let e = expression st in
      expect st (SYMBOL ";") "';'";
      ExprStmt e
  in
  { s = desc; sline }

and expression st = assignment st

and assignment st =
  let target = logical_or st in
  if accept st (SYMBOL "=") then begin
    let value = assignment st in
    match target.e with
    | Var name -> { target with e = Assign (name, value) }
    | Index (obj, idx) -> { target with e = IndexAssign (obj, idx, value) }
    | _ -> raise (Parse_error ("invalid assignment target", target.line))
  end
  else target

and logical_or st =
  let rec loop left =
    if accept st (KEYWORD "or") then loop { left with e = Binary (Or, left, logical_and st) } else left
  in
  loop (logical_and st)

and logical_and st =
  let rec loop left =
    if accept st (KEYWORD "and") then loop { left with e = Binary (And, left, equality st) } else left
  in
  loop (equality st)

and equality st =
  let rec loop left =
    if accept st (SYMBOL "==") then loop { left with e = Binary (Eq, left, comparison st) }
    else if accept st (SYMBOL "!=") then loop { left with e = Binary (Neq, left, comparison st) }
    else left
  in
  loop (comparison st)

and comparison st =
  let rec loop left =
    match (peek st).tok with
    | SYMBOL "<" -> ignore (advance st); loop { left with e = Binary (Lt, left, term st) }
    | SYMBOL "<=" -> ignore (advance st); loop { left with e = Binary (Le, left, term st) }
    | SYMBOL ">" -> ignore (advance st); loop { left with e = Binary (Gt, left, term st) }
    | SYMBOL ">=" -> ignore (advance st); loop { left with e = Binary (Ge, left, term st) }
    | _ -> left
  in
  loop (term st)

and term st =
  let rec loop left =
    if accept st (SYMBOL "+") then loop { left with e = Binary (Add, left, factor st) }
    else if accept st (SYMBOL "-") then loop { left with e = Binary (Sub, left, factor st) }
    else left
  in
  loop (factor st)

and factor st =
  let rec loop left =
    if accept st (SYMBOL "*") then loop { left with e = Binary (Mul, left, unary st) }
    else if accept st (SYMBOL "/") then loop { left with e = Binary (Div, left, unary st) }
    else if accept st (SYMBOL "%") then loop { left with e = Binary (Mod, left, unary st) }
    else left
  in
  loop (unary st)

and unary st =
  let l = line st in
  if accept st (SYMBOL "-") then { e = Unary (Neg, unary st); line = l }
  else if accept st (KEYWORD "not") then { e = Unary (Not, unary st); line = l }
  else postfix st

and postfix st =
  let rec loop callee =
    if accept st (SYMBOL "(") then begin
      let args =
        if accept st (SYMBOL ")") then []
        else begin
          let rec collect acc =
            let a = expression st in
            if accept st (SYMBOL ",") then collect (a :: acc)
            else (expect st (SYMBOL ")") "')'"; List.rev (a :: acc))
          in
          collect []
        end
      in
      loop { callee with e = Call (callee, args) }
    end
    else if accept st (SYMBOL "[") then begin
      let idx = expression st in
      expect st (SYMBOL "]") "']'";
      loop { callee with e = Index (callee, idx) }
    end
    else callee
  in
  loop (primary st)

and primary st =
  let l = line st in
  let mk e = { e; line = l } in
  match (advance st).tok with
  | NUMBER f -> mk (Num f)
  | STRING s -> mk (Str s)
  | KEYWORD "true" -> mk (Bool true)
  | KEYWORD "false" -> mk (Bool false)
  | KEYWORD "nil" -> mk Nil
  | IDENT name -> mk (Var name)
  | SYMBOL "(" ->
    let e = expression st in
    expect st (SYMBOL ")") "')'";
    e
  | SYMBOL "[" ->
    if accept st (SYMBOL "]") then mk (List [])
    else begin
      let rec collect acc =
        let item = expression st in
        if accept st (SYMBOL ",") then collect (item :: acc)
        else (expect st (SYMBOL "]") "']'"; List.rev (item :: acc))
      in
      mk (List (collect []))
    end
  | KEYWORD "fn" ->
    let ps = params st in
    let body = block st in
    mk (Lambda (ps, body))
  | t -> raise (Parse_error ("unexpected " ^ token_to_string t, l))

let parse (src : string) : stmt list =
  let st = { toks = tokenize src } in
  program st
