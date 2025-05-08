# from Ceta 2 to Ceta 3

sematic additions: several, in particular, `coreMatrixinterpretation`

syntactic additions: 
- `lookuptables` to declare identifiers that denote rules
- instead of `rule` can use `ruleInde`

syntactic changes (even if you don't use additions)
- `lookuptables` element is required (it can be empty)
- term syntax changed: removed `arg` nodes inside `funapp`
- rule syntax changed: removed `lhs`, `rhs` nodes inside `rule`
- certification problem: now requires `cpfFormat` as first child
- inside ruleremoval: removed `orderingConstraintProof` and `redPair` node
- `polynomial` node removed
- certificationProblem needs children `property`, `answer`

