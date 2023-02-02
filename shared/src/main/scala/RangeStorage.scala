package be.adamv.cz2


class RangeStorage[A](val start: Int, val end: Int):
  val size = end - start
  assert(size > 0)
  var occupied = 0

  def free: Int = size - occupied

  val valueToIndex = collection.mutable.HashMap.empty[A, Int]
  val indexToValue = collection.mutable.HashMap.empty[Int, A]
  val children = collection.mutable.ListBuffer.empty[RangeStorage[_]]

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
    valueToIndex.get(a) match
      case Some(i) => i
      case None =>
    val i = start + occupied
    valueToIndex(a) = i
    indexToValue(i) = a
    occupied += 1
    i

  def couldContain(i: Int): Boolean =
    i < end && i >= start

  def get(i: Int): Option[A] =
    indexToValue.get(i)

  def lookup(s: A): Option[Int] =
    valueToIndex.get(s)
