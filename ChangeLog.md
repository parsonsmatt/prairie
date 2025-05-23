# Changelog for prairie

## 0.1.0.0

- []()
    - Add `tabulateRecordApply` which is like `tabulateRecordA` but allows you to use non-Applicaive effects.
    - `Prairie.Fold`: `foldMapRecord` now works with any `Semigroup` instead of any `Monoid`, allowing you to fold into a `NonEmpty` list.
    - `Prairie.Traverse`:
        - `traverseRecord_` introduced as a slightly more efficient form of `void . traverseRecord f`.
        - `traverseFields` and `traverseFields_` introduced to only operate on fields, not requiring a whole record.
        - `traverseFieldsWithDict` (and `_` suffix variant) introduced to operate on fields with a constraint available.

## 0.0.4.1

- [#18](https://github.com/parsonsmatt/prairie/pull/18)
    - Drop `lens` dependency

## 0.0.4.0

- [#13](https://github.com/parsonsmatt/prairie/pull/13)
    - Introduce `Prairie.Semigroup`, allowing you to combine two records by semigroup-appending their fields together.
    - Introduce `Prairie.Zip`, allowing you to combine two records by specifying how to combine their fields. This fuels `Prairie.Semigroup`.
    - Introduce `Prairie.Monoid`, allowing you to make an `emptyRecord` with `mempty` at each field.
    - Introduce `Prairie.AsRecord`, allowing you to derive instances for records based on their `Record` and `FieldDict` instances.

## 0.0.3.0

- [#8](https://github.com/parsonsmatt/prairie/pull/8)
    - The `Prairie.Fold` module is introduced, allowing you to fold records.
    - The `Prairie.Traverse` module is introduced, allowing you to traverse over records.

## 0.0.2.1

- [#6](https://github.com/parsonsmatt/prairie/pull/6)
    - Bump upper bound for `TemplateHaskell`, supporting up to GHC 9.8
    - Fix string literals in docs that were quoted as module names

## 0.0.2.0

- [#2](https://github.com/parsonsmatt/prairie/pull/2)
    - Add `tabulateRecordA` to `Record` class. `tabulate` and `allFields` are now normal functions.
    - Provide a default implementation of `recordFieldLabel` for `Show`able fields.

## 0.0.1.1

* [#4](https://github.com/parsonsmatt/prairie/pull/4)
    * Compatibility with `template-haskell-2.18` and above
