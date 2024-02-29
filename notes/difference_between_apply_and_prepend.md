```scala
ExprMap(Expr(a, b, c) -> 1, Expr(A, B, C) -> 2)
  .apply(f) ==
ExprMap(Expr(f, Expr(a, b, c)) -> 1, Expr(f, Expr(A, B, C)) -> 2)
```
```scala
ExprMap(Expr(a, b, c) -> 1, Expr(A, B, C) -> 2)
  .prepend(f) ==
ExprMap(Expr(f, a, b, c) -> 1, Expr(f, A, B, C) -> 2)
```