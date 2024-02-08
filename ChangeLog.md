# Changelog for prairie

## 0.0.2.1

- [#3](https://github.com/parsonsmatt/prairie/pull/6)
    - Bump upper bound for `TemplateHaskell`, supporting up to GHC 9.8
    - Fix string literals in docs that were quoted as module names

## 0.0.2.0

- [#2](https://github.com/parsonsmatt/prairie/pull/2)
    - Add `tabulateRecordA` to `Record` class. `tabulate` and `allFields` are now normal functions.
    - Provide a default implementation of `recordFieldLabel` for `Show`able fields.
    
## 0.0.1.1

* [#4](https://github.com/parsonsmatt/prairie/pull/4)
    * Compatibility with `template-haskell-2.18` and above
