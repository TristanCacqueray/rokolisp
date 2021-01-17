# RokoLisp

A programing language designed for embedded use in applications.

You can think of RokoLisp as: lambda calculus + import + literal + syntax sugar + runtime.

The goal of this project is to explore pure language design that can be reloaded in realtime for instant feedback.

## Usage

```bash
$ echo "(./test/code/fact.rl 4)" | cabal run rokolisp
24
```

Example program: [fact.rl](./test/code/fact.rl).

## Terms

A RokoLisp term can be one of these three things:

- A variable: `x`
- An abstraction: `(λx x)`
- An application: `(f x)`

The term data type is defined as `data Term = Var Name | Lam Name Term | App Term Term`

## Syntax

The core syntax is defined as `data Syntax = Atom Name | List [Syntax]` using s-expressions.
The syntax is desugared to term:

- Abstraction: `(λx x)` -> `(Lam "x" (Var "x"))`
- Abstraction curry: `(λx y x)` -> `(Lam "x" (Lam "y" (Var "x")))`
- Application: `(f x)` -> `(App (Var "f") (Var "x"))`
- Application curry: `(f x y)` -> `(App (App (Var "f") (Var "x")) (Var "y"))`
- Let binding: `(let name value body)` -> `(App (Lam "name" body) value)`
- Imports: `./path` -> `path content`

Comments starting with `;` are ignored.

## Runtime

A RokoLisp term can be evaluated to a value:

- Free variables may become a literal
  - Integer: `"42"` -> `LitInt 42`

The runtime implements the following built-ins:

- church_numeral_encode: `2` -> `(λf (λs (f (f s))))`
- church_numeral_decode: `(λ f x (f (f x)))` -> `LitInt 2`
