package be.adamv

import scala.annotation.tailrec
import scala.collection.mutable

extension [K, V](m1: Map[K, V])
  def merge(op: (V, V) => V)(m2: Map[K, V]): Map[K, V] =
    (m1 -- m2.keySet) ++ m2.map((k, v) => k -> m1.get(k).map(op(v, _)).getOrElse(v))

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
