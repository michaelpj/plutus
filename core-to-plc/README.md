# core-to-plc: GHC Core to Plutus IR compiler and plugin

This package is a library which allows compiling GHC Core into Plutus IR, with the aim of allowing people to write Plutus contracts in surface Haskell.

The package also contains a GHC compiler plugin which looks for specially tagged expressions and compiles them (via Plutus IR) to Plutus Core at compile 
time and embeds the result in the Haskell program for use at runtime. This should not usually be used directly, but rather via the `plutus-th` package.

## Known limitations

The following GHC Haskell language features are not supported by the compiler:

- Datatypes beyond simple "Haskell98" datatypes
    - GADTs, datatype constraints, etc.
- Mutually recursive datatypes (support planned)
    - Self-recursive datatypes are supported
- Abstract data types
- Unbounded `Integer`s, floats and doubles
- Record selectors (support planned)
- Literal patterns
- Typeclasses
    - Some `Num`, `Eq` and `Ord` methods on `Int` and `ByteString` are supported
