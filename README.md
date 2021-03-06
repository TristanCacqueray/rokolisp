# RokoLisp

A programing language designed for embedded use in applications.

You can think of RokoLisp as: lambda calculus + import + literal + syntax sugar + runtime.

The goal of this project is to explore pure language design that can be reloaded in realtime for instant feedback.

## Usage

```bash
$ echo "(./test/code/fact.rkl 8)" | cabal run rokolisp
40320
```

Example program: [fact.rkl](./test/code/fact.rkl).

## Terms

A RokoLisp term can be one of these three things:

- A variable: `x`
- An abstraction: `(λx x)`
- An application: `(f x)`

The term data type is defined as `data Term = Var Name | Lam Name Term | App Term Term`

## Syntax

The core syntax is defined as `data Syntax = Atom Name | List [Syntax]` using s-expressions.
The syntax is desugared to term:

| Name              | Sugar                          | Term                                                                 |
| ----------------- | ------------------------------ | -------------------------------------------------------------------- |
| Abstraction       | `(λx x)`                       | `Lam "x" (Var "x")`                                                  |
| Abstraction curry | `(λx y x)`                     | `Lam "x" (Lam "y" (Var "x"))`                                        |
| Application       | `(f x)`                        | `App (Var "f") (Var "x")`                                            |
| Application curry | `(f x y)`                      | `App (App (Var "f") (Var "x")) (Var "y")`                            |
| Let binding       | `(let name value body)`        | `App (Lam "name" body) value`                                        |
| Let bindings      | `(let n1 v1 n2 v2.. in body )` | `App (Lam "n1" (App (Lam "n2" (Var "body")) (Var "v2"))) (Var "v1")` |
| Do notation       | `(do x y z)`                   | `(>> x (>> y z))`                                                    |
|                   |                                |                                                                      |
| Boolean true      | `true`                         | `Lam "x" (Lam "y" (Var "x"))`                                        |
| Boolean false     | `false`                        | `Lam "x" (Lam "y" (Var "y"))`                                        |
| Pair              | `(cons a b)`                   | `Lam "s" (App (App (Var "s") (Var "a")) (Var "b"))`                  |
| Nil               | `nil`                          | `(λx true)`                                                          |
| List              | `(list 1 2 3)`                 | `(cons 1 (cons 2 (cons 3 nil)))`                                     |
|                   |                                |                                                                      |
| Imports           | `./path`                       | `path content`                                                       |
|                   |                                |                                                                      |

Comments starting with `;` are ignored.

## Runtime

A RokoLisp term can be evaluated to a value:

- Free variables may become a literal
  - `42` -> Integer
  - `"hello"` -> Text

The runtime implements the following built-ins:

- `(church_numeral_encode 2)` -> `(λf (λs (f (f s))))`
- `(church_numeral_decode (λ f x (f (f x))))` -> `2`
- `(equals? 1 1)` -> `(λx (λy x))` if true else `(λx (λy y))`
- `(print 42)` -> io to print 42
- `(>> x y)` -> evaluate the io x then the io y
