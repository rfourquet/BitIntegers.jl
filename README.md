# BitIntegers

[![Tests Status](https://github.com/rfourquet/BitIntegers.jl/workflows/CI/badge.svg)](https://github.com/rfourquet/BitIntegers.jl/actions?query=workflow%3ACI)


This package implements fixed-width integer types similar to standard builtin-ones like `Int` or `UInt128`.
The following types, with obvious meaning, are exported: `Int256`, `UInt256`, `Int512`, `UInt512`, `Int1024`, `UInt1024`;
they come with string macros to construct them (like for `Int128` and `UInt128`), e.g. `int256"123"`.
It's possible to instantiate a new pair of types with the exported `@define_integers` macro:

```julia
julia> BitIntegers.@define_integers 24

julia> UInt24(1), Int24(2)
(0x000001, 2)

julia> BitIntegers.@define_integers 8 MyInt8 MyUInt8

julia> MyUInt8(1)
0x01

julia> myint8"123" # the string macro is named like the type, in lower case
123
```

This is implemented using `primitive type` and julia intrinsics, the caveat being that it might
not always be legal (e.g. in some julia versions, `Primes.factor(rand(UInt256))` used to
make LLVM abort the program, while it was fine for `Int256`).

There are another couple of outstanding issues:

1) the intrinsics for division operations used to make LLVM fail for widths greater than 128 bits,
so they are here implemented via conversion to `BigInt` first, which makes them quite slow;
it got slightly better in recent julia (nightly pre-1.10), where it prints
`JIT session error: Symbols not found: [ __divei4 ]` but at least doesn't abort.

2) prior to Julia version 1.2: for some reason, importing this code invalidates many precompiled
functions from `Base`, so the REPL experience becomes very annoyingly slow until functions get
recompiled (fixed by https://github.com/JuliaLang/julia/pull/30830);

3) prior to Julia version 1.4: creating arrays of types of size not a power of two easily leads
to errors and segfaults (cf. e.g. https://github.com/rfourquet/BitIntegers.jl/issues/1, fixed by
https://github.com/JuliaLang/julia/pull/33283).
