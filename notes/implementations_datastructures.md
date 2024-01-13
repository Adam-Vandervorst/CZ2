hyperon-experimental  uses
```
Atom := Variable UniqueId | Symbol String | Expression (Vector Expr) | Grounded SomeClass
Space := Vector Atom
```
distributed-atomspace  uses
```
Atom := {id: UniqueId, root: Boolean, type: Signature, children: Vector UniqueId}
Space := Table Atom
```
CZ2  uses
```
Atom := NewVar | VarRef Int | Symbol Int | App Atom Atom
ExprMap a := {newvar: Maybe a, varrefs: IntMap a, symbols: IntMap a, apps: ExprMap ExprMap a}
(basic) Space := ExprMap Unit
```