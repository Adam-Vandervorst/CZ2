## Number or parity
```scala
trait Parity
case object Even extends Parity
case object Odd extends Parity

def represent(e: Either[Int, Parity]): Int = e match
  case Left(a) => a
  case Right(Odd) => 1
  case Right(Even) => 2


@main def m =
  val num = 13*17
  val info = if num < 256 then Left(num) else Right(parity(num))
  val res = represent(info)
  println(res)
```


```metta
(: Parity Type)
(: Even Parity)
(: Odd Parity)

(: represent (-> (Either Int Parity) Int))
(= (represent (Left $a)) $a)
(= (represent (Right Odd)) 1)
(= (represent (Right Even)) 2)

!(println! 
  (let $num (* 13 17)
  (let $info (if (< $num 256) (Left $num) (Right (parity $num)))
  (represent $info)
)))
```