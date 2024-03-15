package be.adamv.cz2


class RangeStorage[A](val start: Int, val end: Int):
  val size = end - start
  assert(size > 0)
  var occupied = 0

  def free: Int = size - occupied

  val valueToIndex = collection.mutable.HashMap.empty[A, Int]
  val indexToValue = collection.mutable.HashMap.empty[Int, A]
  val children = collection.mutable.ListBuffer.empty[RangeStorage[_]]

  extension (inline sc: StringContext)(using inline ev: String =:= A)
    inline def v(inline args: Any*): Expr =
      val k = ev(StringContext.standardInterpolator(identity, args, sc.parts))
      Var(this.lookup(k).getOrElse(this.add(k)))

  def inRange[B](size: Int): RangeStorage[B] =
    assert(size <= free)
    val newNS = new RangeStorage[B](start + occupied, start + occupied + size)
    children.addOne(newNS)
    occupied += size
    newNS

  def storeInRange[B](elems: Set[B]): RangeStorage[B] =
    assert(elems.size <= free)
    val newNS = new RangeStorage[B](start + occupied, start + occupied + elems.size)
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

  def couldContain(i: Int): Boolean =
    i < end && i >= start

  def get(i: Int): Option[A] =
    indexToValue.get(i)

  def lookup(s: A): Option[Int] =
    valueToIndex.get(s)

object RangeStorage:
  def highPos[A](): RangeStorage[A] = new RangeStorage[A](2 << 24, 2 << 29)

object Vars:
  def unapplySeq(ns: RangeStorage[Int]): Some[LazyList[Expr]] =
    Some(LazyList.tabulate(ns.free)(ns.addV))
