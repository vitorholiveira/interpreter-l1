# Interpreter for L1

This repository contains the implementation of an interpreter for the L1 language, which includes:

- **Expression evaluator:** Based on the big-step operational semantics defined for L1.
- **Type inference:** Implemented according to the specified type system.

## Supported Syntax

The L1 language supports the following constructs:

- Literal values (`n`, `b`)
- Binary operations (`e1 op e2`)
- Conditional structures (`if e1 then e2 else e3`)
- Variables (`x`)
- Function applications (`e1 e2`)
- Anonymous functions (`fn x:T ⇒ e`)
- Declarations (`let x:T = e1 in e2`, `let rec f:T1 → T2 = (fn y:T1 ⇒ e1) in e2`)
- List operations (`nil`, `::`, `isempty`, `hd`, `tl`)
- Pattern matching (`match ... with`)
- Optional types (`nothing`, `just e`)

## Quick Setup Guide

### Ubuntu/Debian
```bash
# Install dependencies
sudo apt update
sudo apt install opam build-essential make m4

# Initialize OPAM
opam init
eval $(opam env)

# Install OCaml
opam switch create 4.14.0
eval $(opam env)

# Install required packages
opam install ounit2 ocaml-compiler-libs ocamlfind
```

## Compile and run:
```bash
eval $(opam env)
ocamlfind ocamlc -package ounit2 -linkpkg -o tests interpreter.ml tests.ml
./tests
```

## Common Issues

- If OPAM init fails: `opam init --shell-setup`
- Permission issues: `sudo chown -R $(whoami):$(whoami) ~/.opam`
- Missing deps on Ubuntu: `sudo apt install pkg-config libgmp-dev`