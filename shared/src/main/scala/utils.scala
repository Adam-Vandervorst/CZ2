package be.adamv.cz2

import scala.annotation.tailrec
import scala.collection.mutable

extension [V](m1: mutable.LongMap[V])
  def union(m2: mutable.LongMap[V]): mutable.LongMap[V] =
    m1 ++ m2

  def unionWith(op: (V, V) => V)(m2: mutable.LongMap[V]): mutable.LongMap[V] =
    mutable.LongMap.from(m1.view.filterKeys(l => !m2.contains(l)) ++ m2.map((k, v) => k -> m1.get(k).map(op(v, _)).getOrElse(v)))

  def intersection(m2: mutable.LongMap[V]): mutable.LongMap[V] =
    mutable.LongMap.from(m1.view.filterKeys(m2.contains))

  def intersectionWith(op: (V, V) => V)(m2: mutable.LongMap[V]): mutable.LongMap[V] =
    mutable.LongMap.from((m1.keySet intersect m2.keySet).map(k => k -> op(m1(k), m2(k))))

extension [X, CC[_], C](xs: collection.IterableOnceOps[X, CC, C])
  def mapAccumulate[S, Y](initial: S)(fs: (X, S) => (Y, S)): CC[Y] =
    var c = initial
    xs.map { x =>
      val (y, nc) = fs(x, c)
      c = nc
      y
    }

extension [X, CC[_], C <: collection.IterableOps[X, CC, C]](xs: C)
  def asNonEmpty: Option[C] = Option.when(xs.nonEmpty)(xs)

extension (lm: mutable.LongMap.type)
  def single[V](k: Long, v: V): mutable.LongMap[V] =
    val m = mutable.LongMap.empty[V]
    m.update(k, v)
    m

@tailrec
def fix[A](f: A => A)(v: A, prev: Option[A] = None): A =
  if !prev.contains(v) then fix(f)(f(v), Some(v)) else v

@tailrec
def fixproject[A, Q](f: A => A, project: A => Q)(v: A, prev: Option[Q] = None): A =
  val vq = project(v)
  if !prev.contains(vq) then fixproject(f, project)(f(v), Some(vq)) else v

def toLetters(n: Int): String =
  val l = ((n % 26) + 65).toChar.toString
  if n >= 26 then toLetters((n - 26)/26) + l
  else l

def hash(x: Long): Long =
  var r = x
  r = (r ^ (r >>> 30)) * 0xbf58476d1ce4e5b9L
  r = (r ^ (r >>> 27)) * 0x94d049bb133111ebL
  r = r ^ (r >>> 31)
  r

extension [A](bit: collection.BufferedIterator[A])
  // exclusive version of takeWhile
  def takeThrough(p: A => Boolean): Iterator[A] = new collection.AbstractIterator[A] {
    var hdDefined: Boolean = false

    def hasNext = hdDefined || bit.hasNext && {
      if (p(bit.head)) hdDefined = true
      else bit.drop(1)
      hdDefined
    }
    def next() = if (hasNext) { hdDefined = false; bit.next() } else Iterator.empty.next()
  }