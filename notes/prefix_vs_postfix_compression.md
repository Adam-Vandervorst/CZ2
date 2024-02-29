


(f a)
(f b)
(f c)
...
(f omega)

(transform (f $a) (g $a))


(g a)
(g b)
(g c)
...
(g omega)






(f a)
   b)
   c)
...
   omega)

(transform (f $a) (g $a))


(g a)
   b)
   c)
...
   omega)



l = a::b::Nil
l_ = q::l





Database:
(Address, (Street <name>), (Zip <zip>), ..)
(Address, (Street <name>), (Zip <zip>), ..)
(Address, (Street <name>), (Zip <zip>), ..)
(Address, (Street <name>), (Zip <zip>), ..)
Graph Database
(P1 pa pb pc pd)
(P2 qa qb)
(P3 za qb... zomega)
(P3)
AtomSpace
(P1 (Q1 pa pb) pc)
(P2 (Q2 qa qb) pd)
(P3 (Z1 ia) (Z1 ib))
(P3 (Z1 ic) (Z1 ib))


(f 1)
(f 2)
(f 3)

### prefix
```
f a b
f b c
  v
f-a-b
 \
  b-c
```

(f 1)
(g 1)
(h 1)

### postfix
```
f g a
g h a
  v
f-g-a
   /
g-h
```

AtomSpace
### optimal
```
f g a
f h a
  v
f-g-a
 \ /
  h

  (transform (f $a $b) (g $a $b))
  
  (transform (g $a $b) (g $a z))

f
\\
g-g-a
 \ /
  h

g-g-z
 \ /
  h
  
```
