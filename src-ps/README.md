# Purescript Example


## Build

```
> psc-package build
> pulp repl
```

## Further Work

* Document use of ST 
* Explore how to use the Time module, document
* Document results from ST, foldl, and HashMap results


## Performance Results

  Before HashMap change - on GB laptop
  Read Accounts Complete 47.0
  Read Transactions Complete 72.0
  Parse Accounts complete 3569.0
  Accounts Right Complete 145.0
  Create Account Lookup complete 10079.0
  Parse Transaction Complete 4404.0
  Process Transactions Complete 37111.0
  Complete 42447.0
  Total 97874.0
  
  After HashMap change - on GB laptop
  Read Accounts Complete 49.0
  Read Transactions Complete 61.0
  Parse Accounts complete 3731.0
  Accounts Right Complete 152.0
  Create Account Lookup complete 2953.0
  Parse Transaction Complete 4730.0
  Process Transactions Complete 27945.0
  Values ToArray Complete 1017.0
  Complete 31522.0
  Total 72160.0


