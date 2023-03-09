# Lumen

> 🇺🇸 [English version below](#english)

Uma linguagem de programação pequena e dinâmica, com interpretador escrito em OCaml: lexer à mão, parser recursivo, e um tree-walking interpreter com escopo léxico, funções de primeira classe e closures. Só a biblioteca padrão do OCaml, compila com `ocamlopt` puro.

Tem também um jogo escrito *na própria linguagem*: `examples/adventure.lm`, um text adventure de fugir de uma masmorra. É pequeno, mas foi o teste de verdade de que a linguagem serve pra alguma coisa além de `fib(20)`.

```
$ make && ./bin/lumen examples/adventure.lm
== Cell ==
You wake up in a damp cell. A rusty door leads north.
> north
== Corridor ==
A long corridor lit by one torch. A guard room lies east, the cell south, and a gate north.
> east
== Guard room ==
An empty guard room. The guard left in a hurry.
There is a key here.
> take key
You pick up the key.
```

## A linguagem

```
fn make_counter(start) {
  let count = start;
  return fn() { count = count + 1; return count; };
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

- Valores: números (float 64), strings, booleanos, `nil`, listas, funções.
- `let x = 1;` declara, `x = 2;` atribui na declaração mais próxima. Blocos abrem escopo.
- Operadores aritméticos e de comparação, `and or not`, concatenação e repetição de string com `+` e `*`, concatenação de listas.
- `if/else`, `while`, `for` estilo C, `break`, `continue`, `return`.
- Closures capturam por referência; recursão (inclusive mútua) funciona.
- Builtins: `print write input len push pop str num type sqrt floor range join split clock`.
- Erros dizem a linha: `runtime error on line 3: undefined variable 'z'`.

`./bin/lumen` sem argumento abre um REPL onde expressões são impressas direto.

## Por dentro

1. `src/lexer.ml` transforma caracteres em tokens com a linha de cada um.
2. `src/parser.ml` é recursivo, uma função por regra, com precedence climbing (`or` < `and` < igualdade < comparação < `+ -` < `* / %` < unário < chamada/índice). O `for` é açúcar sobre um `while`, mas com um nó próprio pra `continue` funcionar (a primeira versão travava num loop infinito, lição aprendida).
3. `src/interp.ml` anda na AST. Ambientes são hash tables encadeadas; uma closure é o corpo mais o ambiente onde nasceu. `return`, `break` e `continue` são exceções do OCaml capturadas pela chamada ou pelo loop de fora, que é a solução mais simples e a mais rápida que eu achei.

Testes: `make test` roda uns 60 programas e compara a saída, incluindo o adventure com uma lista de comandos como entrada (com e sem pegar a chave). `make examples` roda tudo de `examples/`.

---

## English

A small, dynamic programming language, with an interpreter written in OCaml: hand-made lexer, recursive parser, and a tree-walking interpreter with lexical scope, first-class functions and closures. OCaml standard library only, compiles with plain `ocamlopt`.

There's also a game written *in the language itself*: `examples/adventure.lm`, a text adventure about escaping a dungeon. It's small, but it was the real test that the language is good for something beyond `fib(20)`.

```
$ make && ./bin/lumen examples/adventure.lm
== Cell ==
You wake up in a damp cell. A rusty door leads north.
> north
== Corridor ==
A long corridor lit by one torch. A guard room lies east, the cell south, and a gate north.
> east
== Guard room ==
An empty guard room. The guard left in a hurry.
There is a key here.
> take key
You pick up the key.
```

## The language

```
fn make_counter(start) {
  let count = start;
  return fn() { count = count + 1; return count; };
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

- Values: numbers (64-bit float), strings, booleans, `nil`, lists, functions.
- `let x = 1;` declares, `x = 2;` assigns to the nearest declaration. Blocks open a scope.
- Arithmetic and comparison operators, `and or not`, string concatenation and repetition with `+` and `*`, list concatenation.
- `if/else`, `while`, C-style `for`, `break`, `continue`, `return`.
- Closures capture by reference; recursion (mutual included) works.
- Builtins: `print write input len push pop str num type sqrt floor range join split clock`.
- Errors tell you the line: `runtime error on line 3: undefined variable 'z'`.

`./bin/lumen` with no argument opens a REPL where expressions are printed directly.

## Inside

1. `src/lexer.ml` turns characters into tokens with the line of each one.
2. `src/parser.ml` is recursive, one function per rule, with precedence climbing (`or` < `and` < equality < comparison < `+ -` < `* / %` < unary < call/index). `for` is sugar over a `while`, but with its own node so `continue` works (the first version got stuck in an infinite loop, lesson learned).
3. `src/interp.ml` walks the AST. Environments are chained hash tables; a closure is the body plus the environment it was born in. `return`, `break` and `continue` are OCaml exceptions caught by the call or by the enclosing loop, which is the simplest and fastest solution I found.

Tests: `make test` runs some 60 programs and compares the output, including the adventure with a list of commands as input (with and without taking the key). `make examples` runs everything in `examples/`.

MIT.
