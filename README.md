# The Dendro Programming Language

## Goals

- Fully powered static rank-n type system
- Define everything with one keyword
- First-class modules as blocks
- Pure functional programming subset
- Full compile-time evaluation support

## Example

Fibonacci series:
```dendro
let fib 0 = 0;
let fib 1 = 1;

forall n where n > 1 =>
let fib n = fib (n - 1) + fib (n - 2);

std::io::println "fib {} = {}" 10 (fib 10);
```

Fibonacci series with pattern matching:
```dendro
forall n where n >= 0 =>
let fib n = match n with {
    \0 -> 0,
    \1 -> 1,
    \k -> fib (k - 1) + fib (k - 2),
};

std::io::println "fib {} = {}" 10 (fib 10);
```

Custom integer type:
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

This programming language is currently in the prototype design stage. See [the grammar](boot/parse/src/ast.lalrpop) for more information.
