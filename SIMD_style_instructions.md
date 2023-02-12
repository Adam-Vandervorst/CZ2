
### Prepend
```scala
val em = ExprMap(
      Expr(f, a, b) -> 1,
      Expr(f, b, c) -> 2)
```

`⦑⧼a: ⧼b⦒, b: ⧼c⦒⦒⧽`

 ⦑⦑⧼f: ⧼a: ⧼b⦒, b: ⧼c⦒⦒⦒⧽⧽
    // O(depth=3)
    println(em.prettyStructuredSet())