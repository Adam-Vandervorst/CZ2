# Contents overview

## Expr
The Expr ADT `Expr := Var Int | App Expr Expr` is a binary tree with integer leave nodes.
Utilities are provided for left and right associativity (isomorphisms to `ExprAssoc := Var Int | AppN [ExprAssoc]`), defaulting to left associativity. 
You can see this as the opposite of [S-Expressions](https://en.wikipedia.org/wiki/S-expression), extending the prefix VS postfix choice.

Furthermore, utilities are provided for two different interpretations of the `Var Int`.
- Absolute variables (negative ints) and symbols (positive ints), called the *absolute* representation in code.
This looks like `AExpr := Param UInt | Symbol UInt | App AExpr AExpr` if you wish.
- De Bruijn levels (negative ints referring to the `-i`'th encountered new variable, with `0` introducing a new variable) and symbols (positive ints), called *relative* in code.
This could be encoded as `RExpr := NewParam | ParamRef UInt | App RExpr RExpr`.


## RangeStorage
Using integers as `Expr` leaf nodes make a lot of sense for the computer, but they're not friendly to work with. 
Especially considering that - in any moderately complex kernel - they need to serve multiple purposes.
For example, some integers could represent built in and reserved symbols, others could be assigned to user defined symbols, while any dynamic system also needs symbols for operating.
To avoid collisions, different purposes need their own ranges.
Furthermore, when doing parallel processing, you may want "naming schemes" (e.g. thread one gets the odd numbers while thread two gets the even numbers).
To manage this all, `RangeStorage[Datatype]` provides an abstraction for reserving (sub-)ranges, and store the `Datatype` you want at these addresses.
