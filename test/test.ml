(* Runs Lumen programs and compares their output. Build and run with `make test`. *)

let passed = ref 0
let failed = ref 0

let run src =
  let buf = Buffer.create 256 in
  match Interp.run src (Buffer.add_string buf) with
  | Ok () -> Ok (Buffer.contents buf)
  | Error msg -> Error msg

let expect name src want =
  match run src with
  | Ok got when got = want -> incr passed
  | Ok got ->
    incr failed;
    Printf.printf "FAIL %s\n  got:  %S\n  want: %S\n" name got want
  | Error msg ->
    incr failed;
    Printf.printf "FAIL %s\n  error: %s\n" name msg

let expect_error name src fragment =
  match run src with
  | Error msg when
      let n = String.length fragment in
      let m = String.length msg in
      let rec find i = i + n <= m && (String.sub msg i n = fragment || find (i + 1)) in
      find 0 ->
    incr passed
  | Error msg -> incr failed; Printf.printf "FAIL %s\n  error %S does not mention %S\n" name msg fragment
  | Ok out -> incr failed; Printf.printf "FAIL %s\n  expected an error, got output %S\n" name out

let () =
  (* literals, arithmetic, precedence *)
  expect "numbers" "print 1 + 2 * 3;" "7\n";
  expect "parens" "print (1 + 2) * 3;" "9\n";
  expect "floats" "print 10 / 4; print 7 % 3; print -3 + 1;" "2.5\n1\n-2\n";
  expect "strings" "print \"ab\" + \"cd\"; print \"x\" * 3; print \"n=\" + 5;" "abcd\nxxx\nn=5\n";
  expect "escapes" "print \"a\\tb\\n\" + \"q\\\"\";" "a\tb\nq\"\n";
  expect "booleans" "print true and false; print true or false; print not nil; print 1 < 2; print \"a\" < \"b\";" "false\ntrue\ntrue\ntrue\ntrue\n";
  expect "equality" "print 1 == 1; print \"a\" == \"a\"; print nil == nil; print [1,2] == [1,2]; print 1 != 2;" "true\ntrue\ntrue\ntrue\ntrue\n";
  expect "short circuit" "print false and undefined_variable; print true or undefined_variable;" "false\ntrue\n";
  expect "comments" "# a comment\nprint 1; # trailing\n" "1\n";

  (* variables and scope *)
  expect "let and assign" "let x = 1; x = x + 1; print x;" "2\n";
  expect "block scope" "let x = 1; { let x = 2; print x; } print x;" "2\n1\n";
  expect "assign outer from block" "let x = 1; { x = 5; } print x;" "5\n";

  (* control flow *)
  expect "if else" "let n = 5; if (n > 3) { print \"big\"; } else { print \"small\"; }" "big\n";
  expect "else if" "let n = 2; if (n > 3) print 1; else if (n > 1) print 2; else print 3;" "2\n";
  expect "while" "let i = 0; let s = 0; while (i < 5) { s = s + i; i = i + 1; } print s;" "10\n";
  expect "for" "for (let i = 0; i < 3; i = i + 1) { print i; }" "0\n1\n2\n";
  expect "break continue" "for (let i = 0; i < 10; i = i + 1) { if (i % 2 == 0) continue; if (i > 6) break; print i; }" "1\n3\n5\n";
  expect "nested loops" "let out = \"\"; for (let i = 0; i < 3; i = i + 1) { for (let j = 0; j < 2; j = j + 1) { out = out + str(i) + str(j) + \" \"; } } print out;" "00 01 10 11 20 21 \n";

  (* functions *)
  expect "function" "fn add(a, b) { return a + b; } print add(2, 3);" "5\n";
  expect "recursion" "fn fib(n) { if (n < 2) return n; return fib(n - 1) + fib(n - 2); } print fib(20);" "6765\n";
  expect "implicit nil" "fn f() { } print f();" "nil\n";
  expect "closures" "fn counter() { let c = 0; fn inc() { c = c + 1; return c; } return inc; } let a = counter(); let b = counter(); a(); a(); print a(); print b();" "3\n1\n";
  expect "lambda" "let square = fn(x) { return x * x; }; print square(7); print (fn(a, b) { return a - b; })(10, 4);" "49\n6\n";
  expect "higher order" "fn map(xs, f) { let out = []; for (let i = 0; i < len(xs); i = i + 1) push(out, f(xs[i])); return out; } print map([1, 2, 3], fn(x) { return x * 10; });" "[10, 20, 30]\n";
  expect "early return in loop" "fn find(xs, v) { for (let i = 0; i < len(xs); i = i + 1) { if (xs[i] == v) return i; } return -1; } print find([5, 6, 7], 7); print find([5], 9);" "2\n-1\n";
  expect "mutual recursion" "fn even(n) { if (n == 0) return true; return odd(n - 1); } fn odd(n) { if (n == 0) return false; return even(n - 1); } print even(10); print odd(7);" "true\ntrue\n";
  expect "function value printing" "fn f() {} print f; print type(f); print type(len);" "<fn f>\nfunction\nfunction\n";

  (* lists and builtins *)
  expect "lists" "let xs = [1, \"two\", [3]]; print xs; print len(xs); print xs[1]; xs[0] = 9; print xs[0]; push(xs, 4); print len(xs); print pop(xs); print [1] + [2, 3];" "[1, \"two\", [3]]\n3\ntwo\n9\n4\n4\n[1, 2, 3]\n";
  expect "list aliasing" "let a = [1, 2]; let b = a; push(b, 3); print a;" "[1, 2, 3]\n";
  expect "range join split" "print join(range(5), \"-\"); print join(range(2, 5), \",\"); print split(\"a::b::c\", \"::\"); print len(split(\"x\", \",\"));" "0-1-2-3-4\n2,3,4\n[\"a\", \"b\", \"c\"]\n1\n";
  expect "num str type" "print num(\"42\") + 1; print num(\"zz\"); print type(1); print type(\"s\"); print type(nil); print type([]); print str(1.5) + \"!\";" "43\nnil\nnumber\nstring\nnil\nlist\n1.5!\n";
  expect "math" "print sqrt(16); print floor(3.7); print 2 * 3.5;" "4\n3\n7\n";
  expect "string index" "let s = \"hey\"; print s[0] + s[2]; print len(s);" "hy\n3\n";
  expect "write" "write(\"a\", 1, \"b\"); write(\"\\n\");" "a1b\n";

  (* programs *)
  expect "fizzbuzz"
    "for (let i = 1; i <= 15; i = i + 1) { if (i % 15 == 0) print \"FizzBuzz\"; else if (i % 3 == 0) print \"Fizz\"; else if (i % 5 == 0) print \"Buzz\"; else print i; }"
    "1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\nBuzz\n11\nFizz\n13\n14\nFizzBuzz\n";
  expect "bubble sort"
    "fn sort(xs) { let n = len(xs); for (let i = 0; i < n; i = i + 1) { for (let j = 0; j < n - 1 - i; j = j + 1) { if (xs[j] > xs[j + 1]) { let t = xs[j]; xs[j] = xs[j + 1]; xs[j + 1] = t; } } } return xs; } print sort([5, 2, 9, 1, 5, 6]);"
    "[1, 2, 5, 5, 6, 9]\n";
  expect "deep recursion" "fn sum(n) { if (n == 0) return 0; return n + sum(n - 1); } print sum(5000);" "12502500\n";

  (* errors *)
  expect_error "undefined variable" "print y;" "undefined variable 'y'";
  expect_error "line numbers" "print 1;\nprint 2;\nprint z;" "line 3";
  expect_error "type error" "print 1 + true;" "unsupported operands";
  expect_error "division by zero" "print 1 / 0;" "division by zero";
  expect_error "arity" "fn f(a) {} f(1, 2);" "expects 1 argument(s) but got 2";
  expect_error "call non function" "let x = 3; x();" "cannot call a number";
  expect_error "index out of range" "let l = [1]; print l[5];" "out of range";
  expect_error "syntax" "let = 5;" "syntax error";
  expect_error "unterminated string" "print \"abc;" "unterminated string";
  expect_error "missing paren" "if (true { print 1; }" "expected ')'";
  expect_error "return outside function" "return 1;" "'return' outside";
  expect_error "break outside loop" "break;" "'break' outside";
  expect_error "invalid assignment" "1 = 2;" "invalid assignment target";

  (* input() and the text adventure that ships in examples/ *)
  let feed lines = let rest = ref lines in Interp.input_source := (fun () -> match !rest with [] -> None | l :: tl -> rest := tl; Some l) in
  feed [ "hello"; "world" ];
  expect "input lines" "let a = input(); let b = input(); print a + \" \" + b; print input();" "hello world\nnil\n";
  let adventure = In_channel.with_open_bin "examples/adventure.lm" In_channel.input_all in
  feed [ "look"; "take key"; "north"; "east"; "take key"; "inventory"; "west"; "north"; "open"; "north"; "open" ];
  (match run adventure with
   | Ok out ->
     let has s = let n = String.length s and m = String.length out in
       let rec find i = i + n <= m && (String.sub out i n = s || find (i + 1)) in find 0 in
     if has "nothing here to take" && has "You pick up the key" && has "key" && has "You escaped" then incr passed
     else begin incr failed; Printf.printf "FAIL adventure\n  output: %S\n" out end
   | Error msg -> incr failed; Printf.printf "FAIL adventure\n  error: %s\n" msg);
  feed [ "north"; "north"; "open"; "dance" ];
  (match run adventure with
   | Ok out ->
     let has s = let n = String.length s and m = String.length out in
       let rec find i = i + n <= m && (String.sub out i n = s || find (i + 1)) in find 0 in
     if has "gate is locked" && has "I do not understand" && not (has "You escaped") then incr passed
     else begin incr failed; Printf.printf "FAIL adventure without key\n  output: %S\n" out end
   | Error msg -> incr failed; Printf.printf "FAIL adventure without key\n  error: %s\n" msg);

  Printf.printf "%d passed, %d failed\n" !passed !failed;
  exit (if !failed = 0 then 0 else 1)
