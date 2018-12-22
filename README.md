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
- 10,0000 Transactions

|C#|Rust|PureScript|Haskel|Elixir|
|-|-|-|-|-|
|x?|x?|x?|x?|

## Potential Next Steps

### C#
- Pros
  * Performance without optimization
- Cons
  * Large holes in logic - potential failure points
- [Code](https://github.com/ConcordResearch/LanguageComparison/tree/master/src-cs)

### Rust
- Pros
  * Correctness
- Cons
  - ?
- [Code](https://github.com/ConcordResearch/LanguageComparison/tree/master/src-rs)
### PureScript
- Pros
  * Correctness
- Cons
  * Performance concerns on large datasets (not because of Node)
- [Code](https://github.com/ConcordResearch/LanguageComparison/tree/master/src-ps)
### Haskell
 - Do Account\Transaction problem
 - Look at record syntax
 - What kind of speed to we get?
- [Code](https://github.com/ConcordResearch/LanguageComparison/tree/master/src-hs)
### Elixir
- Pros
    - Concurrent
    - Fault-tolerant
    - Clusteable
- Cons
    - Dynamic
    - Static types (Dyalizer) is an optional add-on
- [Code](https://github.com/ConcordResearch/LanguageComparison/tree/master/src-ex)
