# Monkey in Haskell

An interpreter for the **Monkey** programming language, written in Haskell.

This is my implementation of Thorsten Ball's book
[*Writing An Interpreter In Go*](https://interpreterbook.com/) (v1.7). It follows the structure of the book
(lexer, parser, evaluator, REPL), but is written in idiomatic Haskell rather than being a line-by-line port.
It is an independent learning project and is not affiliated with the book or its author.

## Features

- Integers, booleans and strings
- Arithmetic, comparison and logical operators, with the usual precedence rules
- `let` bindings and `return` statements
- `if` / `else` expressions
- First-class functions, closures and recursion
- Arrays and hashes (with integer, boolean and string keys)
- Builtin functions: `len`, `first`, `last`, `rest`, `push`, `puts`
- An interactive REPL with line editing and history (via [Haskeline](https://hackage.haskell.org/package/haskeline))
- Runtime and parse errors are reported to the user; they do not crash the REPL

## Getting started

You need [Stack](https://docs.haskellstack.org/). The toolchain is pinned by the resolver in `stack.yaml`, so Stack
will install the matching GHC for you.

```sh
git clone https://github.com/TheBlueFireFox/writing-an-interpreter.git
cd writing-an-interpreter

stack build    # compile the library and the executable
stack run      # start the REPL
stack test     # run the test suite
```

In the REPL, type Monkey code and press Enter. Press `Ctrl-D` to exit.

## A quick tour of the language

> Monkey has no comments. The `// =>` annotations below are only here to show results.

### Bindings and expressions

```monkey
let age = 1;
let name = "Monkey";
let result = 10 * (20 / 2);          // => 100
let greeting = "Hello" + ", " + name; // => Hello, Monkey
```

### Arrays and hashes

```monkey
let numbers = [1, 2, 3, 4];
numbers[2];                          // => 3

let person = {"name": "Monkey", "age": 1, true: "yes", 3: "three"};
person["name"];                      // => Monkey
person[3];                           // => three
```

### Functions, recursion and closures

```monkey
let fibonacci = fn(x) {
  if (x < 2) {
    return x;
  }
  fibonacci(x - 1) + fibonacci(x - 2);
};
fibonacci(10);                       // => 55

let newAdder = fn(x) { fn(y) { x + y } };
let addTwo = newAdder(2);
addTwo(3);                           // => 5
```

### Higher-order functions

```monkey
let map = fn(arr, f) {
  let iter = fn(arr, accumulated) {
    if (len(arr) == 0) {
      accumulated
    } else {
      iter(rest(arr), push(accumulated, f(first(arr))));
    }
  };
  iter(arr, []);
};

map([1, 2, 3, 4], fn(x) { x * 2 });  // => [2, 4, 6, 8]
```

### Builtin functions

| Function     | Description                                                     |
| ------------ | --------------------------------------------------------------- |
| `len(x)`     | Length of a string or an array                                  |
| `first(arr)` | First element of an array                                       |
| `last(arr)`  | Last element of an array                                        |
| `rest(arr)`  | A new array with everything except the first element            |
| `push(arr, x)` | A new array with `x` appended (the original is not modified)  |
| `puts(...)`  | Prints each argument on its own line                            |

## Differences from the book

- `first`, `last` and `rest` on an empty array report an error instead of returning `null`.
- `/` truncates toward zero, and division by zero (or overflowing division) is reported as an error rather than
  crashing the interpreter.
- Calling a function with the wrong number of arguments is an error.
- `len` counts characters (Unicode code points) rather than bytes.

## Known limitations

- Input is evaluated one line at a time; there is no multi-line continuation in the REPL yet.
- `==` and `!=` are only supported for integers and booleans.
- Integer literals outside the 64-bit range currently wrap around instead of being rejected.

## Project layout

| Path                   | Purpose                                                                    |
| ---------------------- | -------------------------------------------------------------------------- |
| `app/Main.hs`          | Executable entry point; starts the REPL                                    |
| `src/Token.hs`         | Token types                                                                |
| `src/Lexer.hs`         | Turns source text into tokens                                              |
| `src/Ast.hs`           | AST types, including `Identifier` and `Block`, and the `Display` class     |
| `src/Parser.hs`        | Pratt parser that turns tokens into an AST                                 |
| `src/Object.hs`        | Runtime values, hashable keys and the evaluator's error/return type        |
| `src/Environment.hs`   | Scoped, `IORef`-based environments                                         |
| `src/Evaluator.hs`     | Tree-walking evaluator                                                     |
| `src/Builtin.hs`       | Builtin functions and the global environment                               |
| `src/Repl.hs`          | The read-eval-print loop                                                   |
| `test/`                | [Hspec](https://hspec.github.io/) test suite for each stage                |

## Design notes

- **Errors and `return` are not values.** The evaluator runs in `ExceptT Unwind IO`. A runtime error and a `return`
  statement are both early exits (`Unwind`) that skip the rest of the computation; a function call catches
  `return`, and the top level catches everything else. This means an error can never end up inside an array or a
  variable by accident.
- **Illegal states are unrepresentable where practical.** Function parameters are `Identifier`s, function and `if`
  bodies are `Block`s, and hash keys are a dedicated `HashableObject` type, so the evaluator does not have to handle
  cases the parser can never produce.
- **Environments are mutable references.** Closures capture their environment by reference, which makes recursive
  `let` bindings work without extra machinery.

## Development

The code is formatted with [fourmolu](https://github.com/fourmolu/fourmolu) (see `fourmolu.yaml`). The build uses
`-Wall` and a number of additional warnings; see `package.yaml`.

## License

BSD-3-Clause. See [LICENSE](LICENSE).
