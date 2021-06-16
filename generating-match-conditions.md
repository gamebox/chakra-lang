# Generating match conditions and blocks

## Scalar constants

Scalar constants should be disallowed as the _head_ of a match expression.  Otherwise, we will have to perform calculations to prune dead code branches - and it serves little to no purpose.

## Scalar type values

### NumberType values

Example

```chakra
some-num ?
| 0 -> "None"
| 1 -> "One"
| 2 -> "Two"
| 3 -> "Three"
| _ -> "A lot"
```

Should generate code similar to this:

```llvm
    ; some-num's value is saved to %3 in the entry block
    switch i64 %3, label %3.default []
b3.clause0:
    br %3.return
b3.clause1:
    br %3.return
b4.clause2:
    br %3.return
b4.clause3:
    br %3.return
b4.default:
    br %3.return
b4.return:
    %4 = phi i8* [ @const.None, %b3.clause0 ], [ @const.One, %b3.clause1 ], [ @const.Two, %b3.clause2 ], [ @const.Three, %b3.clause3], [ @const.Many, %b3.default ]
```

Without guards, this should be the simplest case.  Each clause that has a `TCPNumber` pattern type should contribute to the building of an LLVM `switch` instruction, like this.

1. Save the last instruction from the head expression as `exprId`.
2. Fold over the clauses with an initial state of `(exprId, [], irState)`. It will:
   1. match the pattern type
   2. On `TCPNumber`:
      1. Extract the number from the pattern
      2. Create a new basic block
      3. Save the block id as `blockId`
      4. generate the expr list into the new basic block
      5. Save the last expression id as `lastId`
      6. Return a state of `(exprId, (blockId, lastId)::clauseInfo, irState')`
   3. On 'TCVar':
      1. Create a new basic block