data
`(: (f X) (--> A (x A A)))`
pattern (what you're looking for)  template (the shape you want your matches in)
`(: ($f X) (--> $x (x $y $y)))   ~>  (: (inv ($f Z)) (--> (| $y $y) $z))`
result
`(: (inv (f X)) (--> (| A A) A))`
