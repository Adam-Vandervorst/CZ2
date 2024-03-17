package be.adamv.cz2

import scala.annotation.tailrec
import scala.collection.mutable

extension [V](m1: mutable.LongMap[V])
  inline def union(m2: mutable.LongMap[V]): mutable.LongMap[V] =
    val n = m1.clone()
    m2.foreachEntry(n.update)
    n

  def unionWith(op: (V, V) => V)(m2: mutable.LongMap[V]): mutable.LongMap[V] =
    val n = m1.clone()
    m2.foreachEntry((k, v) =>
      var required = true
      val v_ = n.getOrElseUpdate(k, {required = false; v})
      if required then n.update(k, op(v, v_))
    )
    n


  def intersection(m2: mutable.LongMap[V]): mutable.LongMap[V] =
    val n = m1.clone()
    m1.foreachKey(k => if !m2.contains(k) then n.remove(k))
    n

  def intersectionWith(op: (V, V) => V)(m2: mutable.LongMap[V]): mutable.LongMap[V] =
    val n = mutable.LongMap.empty[V]
    m1.foreachEntry((k, v) =>
      m2.get(k) match
        case None => ()
        case Some(v_) => n.update(k, op(v, v_))
    )
    n

  def subtract(m2: mutable.LongMap[V]): mutable.LongMap[V] =
    if m1.isEmpty then mutable.LongMap.empty
    else
      m1.filterNot((k, _) => m2.contains(k))

  def subtractWith(p: (V, V) => Option[V])(m2: mutable.LongMap[V]): mutable.LongMap[V] =
    if m1.isEmpty then mutable.LongMap.empty
    else
      val n = mutable.LongMap.empty[V]
      m1.foreachEntry((k, v) =>
        m2.get(k) match
          case None => n.update(k, v)
          case Some(v_) =>
            p(v, v_) match
              case Some(r) => n.update(k, r)
              case None => ()
      )
      n

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
