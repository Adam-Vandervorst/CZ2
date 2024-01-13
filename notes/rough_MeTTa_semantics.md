```scala
def transformSpace(space: Set[Expr], pattern: Expr, template: Expr): Set[Expr] =
  space.collect{ case e if e unifiable pattern => 
    (App(e, Expr.Any) unification App(pattern, template)).rhs
  }

def lookupMulti(e: Expr)(using s: Set[Expr]): Set[Expr] =
  val n = nameNotIn(e)
  transformSpace(s, Expr(`=`, e, n), n)

def lookupBackupMulti(e: Expr)(using Set[Expr]): Set[Expr] =
  val nv = lookupMulti(e)
  if nv.isEmpty then Set(e)
  else nv

def bottomUpMulti(e: Expr)(using Set[Expr]): Set[Expr] =
  e.foldMap(
    i => lookupBackupMulti(Var(i)), 
    (fs, as) => fs.flatMap(f => as.flatMap(a => lookupBackupMulti(App(f, a))))
  )

def evalMulti(todo: Set[Expr])(using Set[Expr]): Set[Expr] =
  fix(_.flatMap(bottomUpMulti))(todo)
```

A basic 2+2=4 example
```scala
given space: Set[Expr] = Set(
  Expr(`=`, Expr(add, Expr(`,`, Z, $x)), $x)
  Expr(`=`, Expr(add, Expr(`,`, Expr(S, $x), $y)), Expr(S, Expr(add, Expr(`,`, $x, $y))))
)

val two = Expr(S, Expr(S, Z))
val todo = Set(Expr(add, Expr(`,`, two, two))))
val expected = Set(Expr(S, Expr(S, two)))
asssert(evalMulti(todo) == expected)
```

A basic example showcasing the multivalued evaluation
```scala
given space: Set[Expr] = Set(
  Expr(`=`, A, B)
  Expr(`=`, A, C)
  Expr(`=`, Expr(f, B), b)
)

assert(evalMulti(Expr(f, A)) == Set(b, Expr(f, C)))
```
