merging with comment
```
[pane1 pane2] Apply Comment
[pane1 pane2_commented_out] Join
[pane1_with_pane2_commented_out] Return
```

splitting commented into two
```
[pane1_with_pane2_commented_out] Dup
[pane1_with_pane2_commented_out pane1_with_pane2_commented_out] Unapply Comment
[pane1_with_pane2_commented_out pane2] Unapply Comment
```


```
[todolist, readinglist] transformJoin
[doablelist] rankPriority
[listfortoday]
```


pane1
```
(groceries done)
(call doing)
(work pending)
```

pane2
```
(SevenSketches (page 20))
(Topology101 (page 0))
```

result pane
```
($item pending) ~> (Task $item)
($book (page 0)) ~> (ToRead $book)
```