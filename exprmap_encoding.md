If you write the following in code
```
val em = ExprMap(
  Expr(A, a, $, Expr(f, Expr(g, $))) -> 1,
  Expr(A, $, Expr(f, $), Expr(f, _2)) -> 2)
```
it represents a space including the following atoms
```
(A ◆ (f ◆) (f ⏴₂))
(A a ◆ (f (g ◆)))
```
but encoded as
```
⦑⦑⦑⧼A: ⧼a: ⧼◆: ⦑⧼f: ⦑⧼g: ⧼◆⦒⦒⧽⦒⧽⦒,
      ◆: ⦑⧼f: ⧼◆: ⦑⧼f: ⧼⏴₂⦒⦒⧽⦒⦒⧽⦒⦒⧽⧽⧽
```
or in the longform:
```
ExprMap(EM(ExprMap(EM(ExprMap(EM(ExprMap(EM(ExprMap(), LongMap(
    A -> ExprMap(EM(ExprMap(),LongMap(
      a -> ExprMap(EM(ExprMap(), LongMap(
        $ -> ExprMap(EM(ExprMap(EM(ExprMap(), LongMap(
          f -> ExprMap(EM(ExprMap(EM(ExprMap(), LongMap(
            g -> ExprMap(EM(ExprMap(), LongMap(
              $ -> 1)))))), LongMap()))))), LongMap()))))),
      $ -> ExprMap(EM(ExprMap(EM(ExprMap(), LongMap(
        f -> ExprMap(EM(ExprMap(), LongMap(
          $ -> ExprMap(EM(ExprMap(EM(ExprMap(), LongMap(
            f -> ExprMap(EM(ExprMap(), LongMap(
              _2 -> 2)))))), LongMap())))))))), LongMap())))))))),LongMap())),LongMap())),LongMap()))
```