package be.adamv.cz2


trait RangeStorage[A](val start: Int, val end: Int):
  val size: Int = end - start

  def occupied: Int
  def free: Int = size - occupied

  def couldContain(i: Int): Boolean =
    i < end && i >= start

  def get(i: Int): Option[A]

  def lookup(s: A): Option[Int]


class GrowableRangeStorage[A](override val start: Int, override val end: Int) extends RangeStorage[A](start, end):
  assert(size > 0)
  var occupied = 0

  val valueToIndex = collection.mutable.HashMap.empty[A, Int]
  val indexToValue = collection.mutable.HashMap.empty[Int, A]
  val children = collection.mutable.ListBuffer.empty[RangeStorage[_]]

  extension (inline sc: StringContext)(using inline ev: String =:= A)
    inline def v(inline args: Any*): Expr =
      Var(this.lookup(ev(StringContext.standardInterpolator(identity, args, sc.parts))).get)

  def importMap(m: Map[Int, A]): Unit =
    var c = 0
    for (k, v) <- m do
      indexToValue.updateWith(k){
        case None => c += 1; Some(v)
        case Some(v_) if v == v_ => Some(v)
        case Some(v_) => throw RuntimeException(f"$v_ is stored at $k which does not conform the map ($v)")
      }
      valueToIndex.updateWith(v){
        case None => Some(k)
        case Some(k_) if k == k_ => Some(k)
        case Some(k_) => throw RuntimeException(f"$v already has a key $k_ which does not conform to the map ($k)")
      }
    occupied += c

  def inSubRange[B](start: Int, end: Int): GrowableRangeStorage[B] =
    assert(size <= (end - start))
    assert((start to end).forall(get(_).isEmpty))
    val newNS = new GrowableRangeStorage[B](start, end)
    children.addOne(newNS)
    occupied += size
    newNS

  def inRange[B](size: Int): GrowableRangeStorage[B] =
    assert(size <= free)
    val newNS = new GrowableRangeStorage[B](start + occupied, start + occupied + size)
    children.addOne(newNS)
    occupied += size
    newNS

  def storeInRange[B](elems: Set[B]): GrowableRangeStorage[B] =
    assert(elems.size <= free)
    val newNS = new GrowableRangeStorage[B](start + occupied, start + occupied + elems.size)
    children.addOne(newNS)
    occupied += elems.size
    for elem <- elems do newNS.add(elem)
    newNS

  def add(a: A): Int =
    assert(free > 0)
    valueToIndex.getOrElse(a, {
      val i = start + occupied
      valueToIndex(a) = i
      indexToValue(i) = a
      occupied += 1
      i
    })

  def addV(a: A): Expr =
    Expr.Var(add(a))

  def get(i: Int): Option[A] =
    indexToValue.get(i)

  def lookup(s: A): Option[Int] =
    valueToIndex.get(s)

object RangeStorage:
  def highPos[A](): GrowableRangeStorage[A] = new GrowableRangeStorage[A](2 << 24, 2 << 29)
