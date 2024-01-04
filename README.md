# The Dendro Programming Language

## Goals

- Fully powered static rank-n type system
- Define everything with one keyword
- First-class modules as blocks
- Pure functional programming subset
- Full compile-time evaluation support

## Example

```dendro
#[alias]
let { * } = `{
    ..std::ops::{ Add },
};

let MyInt = u32;

let add: Add MyInt MyInt = Add `{
    output: MyInt,

    forall a, b where a: MyInt, b: MyInt =>
    (+) (MyInt a) (MyInt b): MyInt (a + b),
};

let a = MyInt 1;
std::io::println "Hello, {}" (a + (MyInt 2))
```

## Current Stage

This programming language is currently in the prototype design stage. See [the grammar](compiler/parse/src/ast.lalrpop) for more information.
