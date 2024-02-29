The basic idea is to split an expression into multiple ones based on a symbol, say `|`
e.g. `(P (| (S Z) (S (S Z)) Z))` roughly becomes `{P: {S: {S: Z, Z}, Z}}` which is the prefix compression of
`(P (S Z)); (P (S (S Z))); (P Z)`

```scala
val | = Var(1000)
val p1 = Expr(A, Expr(|, Expr(B, C), Expr(|, Expr(B, Expr(B, C)), C)))
val p2 = Expr(Expr(|, f, Expr(h, g)), Expr(|, A, Expr(B, A)))

val em2 = ExprMap(
Expr(f, A) -> 0,
Expr(f, Expr(B, A)) -> 10,
Expr(Expr(h, g), A) -> 1,
Expr(Expr(h, g), Expr(B, A)) -> 11,
)
```

Yikes! The ExprMap way of compressing terms duplicates the `A | (B A)`
`println(em2.prettyStructuredSet())`
`⦑⧼1: ⧼20|⧼21: ⧼20⦒⦒⧽|⧼3: ⧼2: ⧼20|⧼21: ⧼20⦒⦒⧽⦒⦒⧽⧽`


Potential partial parallel transformation in MeTTa
```scala
val fem = ExprMap(
  Expr(`=`, Expr(f, Expr(|, $, $)), _1) -> 0
  Expr(`=`, Expr(f, Expr(|, $, $)), _2) -> 1
)
```
