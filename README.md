# Language Performance Comparison

## Purpose

Understand the performance differences between several languages, and illustrate symantic differences between the languages.

Languages being compared:

1. [C#](#c#)
2. [Rust](#rust)
3. [PureScript](#PureScript)
4. [Haskell](#haskell)
5. [Elixir](#Elixir)

## Performance Comparison
- 1.2M Accounts
- 10M Transactions

|Machine|C#|Rust|PureScript|Haskell|Elixir|
|-|-|-|-|-|-|
|Alfredo|?|186.99s|?|123.34s|187.9s|
|Dave|42s|29s|x|108s|360s|
|(Memory)|4 GB|1.6 GB|x|12 GB|3 GB|

## Potential Next Steps

### C#
- Pros
  * Performance without optimization
- Cons
  * Large holes in logic - potential failure points
- [Code](https://github.com/ConcordResearch/LanguageComparison/tree/master/src-cs)
- Running: `dotnet build; time dotnet run`

### Rust
- Pros
  * Correctness
  * Tool ecosystem
- Cons
  * Borrow checker / need to understand memory layout
  * Lack of libraries
- [Code](https://github.com/ConcordResearch/LanguageComparison/tree/master/src-rs)
- Running: `cargo build --release; time cargo run --release`
### PureScript
- Pros
  * Correctness
- Cons
  * Performance concerns on large datasets (not because of Node)
- [Code](https://github.com/ConcordResearch/LanguageComparison/tree/master/src-ps)
- Running: `pulp build; time node -e "require('output/Main').main()"
### Haskell
 - Do Account\Transaction problem
 - Look at record syntax
 - What kind of speed to we get?
- [Code](https://github.com/ConcordResearch/LanguageComparison/tree/master/src-hs)
- Running: `stack build --fast; time stack run`
### Elixir
- Pros
    - Concurrent
    - Fault-tolerant
    - Clusteable
- Cons
    - Dynamic
    - Static types (Dyalizer) are an optional add-on
- [Code](https://github.com/ConcordResearch/LanguageComparison/tree/master/src-ex)
- Running: `mix deps.get; mix clean; MIX_ENV=prod mix release;mv _build/prod/rel/test_elixir . ; ./test_elixir/bin/test_elixir foreground`
