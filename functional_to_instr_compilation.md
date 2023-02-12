Given
`Nat := Zero | Successor Nat`


### Successor
`succ a = (Successor a)`

```
Apply(Successor)
```
Which can be embedded in ExprMap in Scala...
```scala
val em = ExprMap(
  Expr(succ, 1, 1, Expr(Apply, Successor))
)
```

### Predecessor
`pred (Successor a) = a`

`Unapply(Successor)`


### Subtract10
`subtract10 (Successor (Successor (Successor ... a)...)) = a`

```
loop 10
  Unapply(Successor)
```


TODO: translate
```
add a Zero = a
add a (Successor b) = add (succ a) b
```

### Off-By-At-Most-2
A multivalued function
```
offByAtMost2 (Successor (Successor n)) = n
offByAtMost2 (Successor n) = n
offByAtMost2 n = n
offByAtMost2 n = (Successor n)
offByAtMost2 n = (Successor (Successor n))
```
Where `offByAtMost2 (S (S (S Z)))`
gives you `(S Z), (S (S Z)), (S (S (S Z))), (S (S (S (S Z)))), (S (S (S (S Z))))`

```
[n] Dup
[n n] Unapply Successor
[n pn] Dup
[n pn pn] Unapply Successor
[n pn ppn] Union
[n pn_ppn] Over
[n pn_ppn n] Apply Successor
[n pn_ppn sn] Dup
[n pn_ppn sn sn] Apply Successor
[n pn_ppn sn ssn] Union
[n pn_ppn sn_ssn] Union
[n pn_ppn_sn_ssn] Union
[n_pn_ppn_sn_ssn] Return
```
