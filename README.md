# Lumen

A small dynamically typed programming language, implemented from scratch in
OCaml: a hand-written lexer, a recursive-descent parser and a tree-walking
interpreter with lexical scoping, first-class functions and closures.
Only the OCaml standard library is used; it builds with `ocamlopt` alone.

```
# examples/closures.lm
fn make_counter(start) {
  let count = start;
  return fn() {
    count = count + 1;
    return count;
  };
}

let next = make_counter(10);
next(); next();
print "count is " + next();      # count is 13

fn fib(n) { if (n < 2) return n; return fib(n - 1) + fib(n - 2); }
print fib(20);                   # 6765

let squares = [];
for (let i = 1; i <= 5; i = i + 1) push(squares, i * i);
print squares;                   # [1, 4, 9, 16, 25]
```

```sh
make                 # builds bin/lumen
./bin/lumen examples/fib.lm
./bin/lumen          # REPL: expressions are printed, statements end with ';'
make test
```

## The language

- Values: numbers (64-bit floats), strings, booleans, `nil`, lists, functions.
- `let x = 1;` declares, `x = 2;` assigns (to the nearest enclosing declaration).
- Operators: `+ - * / %`, `== != < <= > >=`, `and or not`, string
  concatenation with `+`, string repetition with `*`, list concatenation.
- Control flow: `if / else`, `while`, C-style `for`, `break`, `continue`,
  `return`. Blocks introduce scopes.
- Functions: `fn name(a, b) { ... }` or anonymous `fn(a) { ... }`; closures
  capture variables by reference, recursion and mutual recursion work.
- Lists: `[1, "two", [3]]`, indexing `xs[i]`, assignment `xs[i] = v`,
  `push`, `pop`, `len`, `range`, `join`, `split`.
- Builtins: `print`, `write`, `len`, `push`, `pop`, `str`, `num`, `type`,
  `sqrt`, `floor`, `range`, `join`, `split`, `clock`.
- Errors report the line: `runtime error on line 3: undefined variable 'z'`.

## How it works

1. **Lexer** (`src/lexer.ml`) scans characters into tokens (numbers,
   strings with escapes, identifiers, keywords, symbols) and records the line
   of each.
2. **Parser** (`src/parser.ml`) is a recursive-descent parser: one function
   per grammar rule, with precedence climbing for binary operators
   (`or` < `and` < equality < comparison < `+ -` < `* / %` < unary <
   call/index). `for` loops are desugared into a block with a `while`.
3. **Interpreter** (`src/interp.ml`) walks the AST. Environments are hash
   tables chained to their parent scope; a closure is the function body
   plus the environment it was created in. `return`, `break` and
   `continue` are OCaml exceptions caught by the enclosing call or loop.
   Built-ins are OCaml functions registered in the global environment.

## Tests

`make test` runs about 60 programs through the interpreter and compares
their output, including closures, recursion, higher-order functions, list
mutation and aliasing, and every category of error.

## License

MIT
