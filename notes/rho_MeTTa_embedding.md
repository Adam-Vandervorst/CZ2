
```lisp
; send the payload "Launch" on the rocket channel
(recv Zero countdown (send rocket Launch))
; send the decremented count to the countdown channel
(recv (Suc $x) countdown (send countdown $x))
; send every count to the announce channel too
(recv $x countdown (send anounce (toString $x)))
; start the system; send the payload "3" on the countdown channel
(send countdown (Suc (Suc (Suc Zero))))

(= step (transform (send $channel $payload) (transform (recv $payload $channel $body) $body)))
```