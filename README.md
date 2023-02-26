# Lumen

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

**EN:** a small dynamically typed language implemented in OCaml (hand-written lexer, recursive-descent parser with precedence climbing, tree-walking interpreter with lexical scope and closures), plus a text adventure written in the language itself. `make test` runs ~60 programs, including the game driven by scripted input. MIT.
