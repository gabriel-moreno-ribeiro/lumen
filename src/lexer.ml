(* Hand-written lexer: turns source text into a list of tokens with line numbers. *)

type token =
  | NUMBER of float
  | STRING of string
  | IDENT of string
  | KEYWORD of string   (* let fn if else while for return print true false nil and or not break continue *)
  | SYMBOL of string    (* ( ) { } [ ] , ; = == != < <= > >= + - * / % *)
  | EOF

type located = { tok : token; line : int }

exception Lex_error of string * int

let keywords =
  [ "let"; "fn"; "if"; "else"; "while"; "for"; "return"; "print"; "true"; "false"; "nil";
    "and"; "or"; "not"; "break"; "continue" ]

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_alnum c = is_alpha c || is_digit c

let tokenize (src : string) : located list =
  let n = String.length src in
  let tokens = ref [] in
  let line = ref 1 in
  let push tok = tokens := { tok; line = !line } :: !tokens in
  let i = ref 0 in
  while !i < n do
    let c = src.[!i] in
    if c = '\n' then (incr line; incr i)
    else if c = ' ' || c = '\t' || c = '\r' then incr i
    else if c = '#' then begin
      while !i < n && src.[!i] <> '\n' do incr i done
    end
    else if is_digit c then begin
      let start = !i in
      while !i < n && (is_digit src.[!i] || src.[!i] = '.') do incr i done;
      let text = String.sub src start (!i - start) in
      match float_of_string_opt text with
      | Some f -> push (NUMBER f)
      | None -> raise (Lex_error ("bad number " ^ text, !line))
    end
    else if is_alpha c then begin
      let start = !i in
      while !i < n && is_alnum src.[!i] do incr i done;
      let word = String.sub src start (!i - start) in
      if List.mem word keywords then push (KEYWORD word) else push (IDENT word)
    end
    else if c = '"' then begin
      let buf = Buffer.create 16 in
      incr i;
      let closed = ref false in
      while !i < n && not !closed do
        let d = src.[!i] in
        if d = '"' then (closed := true; incr i)
        else if d = '\\' && !i + 1 < n then begin
          (match src.[!i + 1] with
           | 'n' -> Buffer.add_char buf '\n'
           | 't' -> Buffer.add_char buf '\t'
           | '"' -> Buffer.add_char buf '"'
           | '\\' -> Buffer.add_char buf '\\'
           | other -> Buffer.add_char buf '\\'; Buffer.add_char buf other);
          i := !i + 2
        end
        else begin
          if d = '\n' then incr line;
          Buffer.add_char buf d;
          incr i
        end
      done;
      if not !closed then raise (Lex_error ("unterminated string", !line));
      push (STRING (Buffer.contents buf))
    end
    else begin
      let two = if !i + 1 < n then String.sub src !i 2 else "" in
      if List.mem two [ "=="; "!="; "<="; ">=" ] then (push (SYMBOL two); i := !i + 2)
      else if String.contains "(){}[],;=<>+-*/%" c then (push (SYMBOL (String.make 1 c)); incr i)
      else raise (Lex_error (Printf.sprintf "unexpected character '%c'" c, !line))
    end
  done;
  push EOF;
  List.rev !tokens

let token_to_string = function
  | NUMBER f -> Printf.sprintf "%g" f
  | STRING s -> Printf.sprintf "%S" s
  | IDENT s -> s
  | KEYWORD s -> s
  | SYMBOL s -> s
  | EOF -> "end of input"
