/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package be.adamv.cz2


import scala.collection.mutable.{Builder, ImmutableBuilder}
import scala.collection.immutable.{AbstractMap, StrictOptimizedMapOps}
import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{AbstractIterator, BuildFrom, Factory, View}
import scala.language.implicitConversions

object VarMapUtils {
  inline def zero(i: Int, mask: Int) = (i & mask) == 0

  inline def mask(i: Int, mask: Int) = i & (complement(mask - 1) ^ mask)

  inline def hasMatch(key: Int, prefix: Int, m: Int) = mask(key, m) == prefix

  inline def unsignedCompare(i: Int, j: Int) = (i < j) ^ (i < 0) ^ (j < 0)

  inline def shorter(m1: Int, m2: Int) = unsignedCompare(m2, m1)

  inline def complement(i: Int) = (-1) ^ i

  inline def bits(num: Int) = 31 to 0 by -1 map (i => (num >>> i & 1) != 0)

  inline def bitString(num: Int, sep: String = "") = bits(num) map (b => if (b) "1" else "0") mkString sep

  inline def highestOneBit(j: Int) = java.lang.Integer.highestOneBit(j)
  
  inline def branchMask(i: Int, j: Int) = highestOneBit(i ^ j)

  inline def join[T](p1: Int, t1: VarMap[T], p2: Int, t2: VarMap[T]): VarMap[T] = {
    val m = branchMask(p1, p2)
    val p = mask(p1, m)
    if (zero(p1, m)) VarMap.Bin(p, m, t1, t2)
    else VarMap.Bin(p, m, t2, t1)
  }

  inline def bin[T](prefix: Int, mask: Int, left: VarMap[T], right: VarMap[T]): VarMap[T] = (left, right) match {
    case (left, VarMap.Nil) => left
    case (VarMap.Nil, right) => right
    case (left, right) => VarMap.Bin(prefix, mask, left, right)
  }
}

import VarMapUtils._

/** A companion object for integer maps.
 *
 *  @define Coll  `IntMap`
 */
object VarMap {
  inline def empty[T] : VarMap[T]  = VarMap.Nil

  inline def singleton[T](key: Int, value: T): VarMap[T] = VarMap.Tip(key, value)

  def apply[T](elems: (Int, T)*): VarMap[T] =
    elems.foldLeft(empty[T])((x, y) => x.updated(y._1, y._2))

  def from[V](coll: IterableOnce[(Int, V)]): VarMap[V] =
    var vm = VarMap.empty[V]
    val it = coll.iterator
    while it.hasNext do
      val (k, v) = it.next()
      vm = vm.updated(k, v)
    vm

  case object Nil extends VarMap[Nothing] {
    // Important! Without this equals method in place, an infinite
    // loop from Map.equals => size => pattern-match-on-Nil => equals
    // develops.  Case objects and custom equality don't mix without
    // careful handling.
//    def equals(that : Any) = that match {
//      case Nil => true
//      case _ => false
//    }
  }

  case class Tip[+T](key: Int, value: T) extends VarMap[T]{
    def withValue[S](s: S) =
      if (s.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this.asInstanceOf[VarMap.Tip[S]]
      else VarMap.Tip(key, s)
  }

  case class Bin[+T](var prefix: Int, var mask: Int, var left: VarMap[T @uncheckedVariance], var right: VarMap[T @uncheckedVariance]) extends VarMap[T] {
    def bin[S](left: VarMap[S], right: VarMap[S]): VarMap[S] = {
      if ((this.left eq left) && (this.right eq right)) this.asInstanceOf[VarMap.Bin[S]]
      else VarMap.Bin[S](prefix, mask, left, right)
    }

    inline def joinIn(p1: Int, t1: VarMap[T @uncheckedVariance], p2: Int): Unit = {
      val m = branchMask(p1, p2)
      val p = VarMapUtils.mask(p1, m)
      if (zero(p1, m))
        val t = this.copy()
        this.prefix = p
        this.mask = m
        this.left = t1
        this.right = t
      else
        val t = this.copy()
        this.prefix = p
        this.mask = m
        this.left = t
        this.right = t1
    }
  }

}

// Iterator over a non-empty IntMap.
abstract class IntMapIterator[V, T](it: VarMap[V]) extends AbstractIterator[T] {

  // Basically this uses a simple stack to emulate conversion over the tree. However
  // because we know that Ints are at least 32 bits we can have at most 32 IntMap.Bins and
  // one IntMap.Tip sitting on the tree at any point. Therefore we know the maximum stack
  // depth is 33 and
  var index = 0
  var buffer = new Array[AnyRef](33)

  def pop = {
    index -= 1
    buffer(index).asInstanceOf[VarMap[V]]
  }

  def push(x: VarMap[V]): Unit = {
    buffer(index) = x.asInstanceOf[AnyRef]
    index += 1
  }
  push(it)

  /**
   * What value do we assign to a tip?
   */
  def valueOf(tip: VarMap.Tip[V]): T

  def hasNext = index != 0
  @tailrec
  final def next(): T =
    pop match {
      case VarMap.Bin(_,_, t@VarMap.Tip(_, _), right) => {
        push(right)
        valueOf(t)
      }
      case VarMap.Bin(_, _, left, right) => {
        push(right)
        push(left)
        next()
      }
      case t@VarMap.Tip(_, _) => valueOf(t)
      // This should never happen. We don't allow IntMap.Nil in subtrees of the IntMap
      // and don't return an IntMapIterator for IntMap.Nil.
      case VarMap.Nil => throw new IllegalStateException("Empty maps not allowed as subtrees")
    }
}

class IntMapEntryIterator[V](it: VarMap[V]) extends IntMapIterator[V, (Int, V)](it) {
  inline def valueOf(tip: VarMap.Tip[V]) = (tip.key, tip.value)
}

class IntMapValueIterator[V](it: VarMap[V]) extends IntMapIterator[V, V](it) {
  inline def valueOf(tip: VarMap.Tip[V]) = tip.value
}

class IntMapKeyIterator[V](it: VarMap[V]) extends IntMapIterator[V, Int](it) {
  inline def valueOf(tip: VarMap.Tip[V]) = tip.key
}

import VarMap._

/** Specialised immutable map structure for integer keys, based on
 *  [[https://ittc.ku.edu/~andygill/papers/IntMap98.pdf Fast Mergeable Integer Maps]]
 *  by Okasaki and Gill. Essentially a trie based on binary digits of the integers.
 *
 *  '''Note:''' This class is as of 2.8 largely superseded by HashMap.
 *
 *  @tparam T    type of the values associated with integer keys.
 *
 *  @define Coll `immutable.IntMap`
 *  @define coll immutable integer map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
sealed abstract class VarMap[+T]  {
  inline def empty: VarMap[T] = VarMap.Nil

  def copy(): VarMap[T] = VarMap.from(this.iterator)

  /**
   * Iterator over key, value pairs of the map in unsigned order of the keys.
   *
   * @return an iterator over pairs of integer keys and corresponding values.
   */
  inline def iterator: Iterator[(Int, T)] = this match {
    case VarMap.Nil => Iterator.empty
    case _ => new IntMapEntryIterator(this)
  }

  final def updatedWith[T1 >: T](k: Int)(remap: Option[T] => Option[T1]): VarMap[T1] =
    this.get(k) match
      case None => remap(None) match
        case None => this
        case Some(v) => this.updated(k, v)
      case Some(v) => remap(Some(v)) match
        case None => this.removed(k)
        case Some(v_) => if v.asInstanceOf[AnyRef] eq v_.asInstanceOf[AnyRef] then this else this.updated(k, v_)

  /**
   * Loops over the key, value pairs of the map in unsigned order of the keys.
   */
  final def foreach[U](f: ((Int, T)) => U): Unit = this match {
    case VarMap.Bin(_, _, left, right) => { left.foreach(f); right.foreach(f) }
    case VarMap.Tip(key, value) => f((key, value))
    case VarMap.Nil =>
  }

  def foreachEntry[U](f: (Int, T) => U): Unit = this match {
    case VarMap.Bin(_, _, left, right) => { left.foreachEntry(f); right.foreachEntry(f) }
    case VarMap.Tip(key, value) => f(key, value)
    case VarMap.Nil =>
  }

  inline def keysIterator: Iterator[Int] = this match {
    case VarMap.Nil => Iterator.empty
    case _ => new IntMapKeyIterator(this)
  }

  /**
   * Loop over the keys of the map. The same as `keys.foreach(f)`, but may
   * be more efficient.
   *
   * @param f The loop body
   */
  final def foreachKey[U](f: Int => U): Unit = this match {
    case VarMap.Bin(_, _, left, right) => { left.foreachKey(f); right.foreachKey(f) }
    case VarMap.Tip(key, _) => f(key)
    case VarMap.Nil =>
  }

  inline def valuesIterator: Iterator[T] = this match {
    case VarMap.Nil => Iterator.empty
    case _ => new IntMapValueIterator(this)
  }

  /**
   * Loop over the values of the map. The same as `values.foreach(f)`, but may
   * be more efficient.
   *
   * @param f The loop body
   */
  final def foreachValue[U](f: T => U): Unit = this match {
    case VarMap.Bin(_, _, left, right) => { left.foreachValue(f); right.foreachValue(f) }
    case VarMap.Tip(_, value) => f(value)
    case VarMap.Nil =>
  }

  protected[this] def className = "IntMap"

  inline def isEmpty = this eq VarMap.Nil
  inline def knownSize: Int = if (isEmpty) 0 else -1
  final def filter(f: (Int, T) => Boolean): VarMap[T] = this match {
    case VarMap.Bin(prefix, mask, left, right) => {
      val (newleft, newright) = (left.filter(f), right.filter(f))
      if ((left eq newleft) && (right eq newright)) this
      else bin(prefix, mask, newleft, newright)
    }
    case VarMap.Tip(key, value) =>
      if (f(key, value)) this
      else VarMap.Nil
    case VarMap.Nil => VarMap.Nil
  }

  final def transform[S](f: (Int, T) => S): VarMap[S] = this match {
    case b@VarMap.Bin(prefix, mask, left, right) => b.bin(left.transform(f), right.transform(f))
    case t@VarMap.Tip(key, value) => t.withValue(f(key, value))
    case VarMap.Nil => VarMap.Nil
  }

  final def size: Int = this match {
    case VarMap.Nil => 0
    case VarMap.Tip(_, _) => 1
    case VarMap.Bin(_, _, left, right) => left.size + right.size
  }

  @tailrec
  final def contains(key: Int): Boolean = this match {
    case VarMap.Bin(prefix, mask, left, right) => if (zero(key, mask)) left.contains(key) else right.contains(key)
    case VarMap.Tip(key2, value) => key == key2
    case VarMap.Nil => false
  }

  @tailrec
  final def get(key: Int): Option[T] = this match {
    case VarMap.Bin(prefix, mask, left, right) => if (zero(key, mask)) left.get(key) else right.get(key)
    case VarMap.Tip(key2, value) => if (key == key2) Some(value) else None
    case VarMap.Nil => None
  }

  @tailrec
  final def getOrElse[S >: T](key: Int, default: => S): S = this match {
    case VarMap.Nil => default
    case VarMap.Tip(key2, value) => if (key == key2) value else default
    case VarMap.Bin(prefix, mask, left, right) =>
      if (zero(key, mask)) left.getOrElse(key, default) else right.getOrElse(key, default)
  }

  @tailrec
  final def apply(key: Int): T = this match {
    case VarMap.Bin(prefix, mask, left, right) => if (zero(key, mask)) left(key) else right(key)
    case VarMap.Tip(key2, value) => if (key == key2) value else throw new IllegalArgumentException("Key not found")
    case VarMap.Nil => throw new IllegalArgumentException("key not found")
  }

  final def updated[S >: T](key: Int, value: S): VarMap[S] = this match {
    case VarMap.Bin(prefix, mask, left, right) =>
      if (!hasMatch(key, prefix, mask)) join(key, VarMap.Tip(key, value), prefix, this)
      else if (zero(key, mask)) VarMap.Bin(prefix, mask, left.updated(key, value), right)
      else VarMap.Bin(prefix, mask, left, right.updated(key, value))
    case VarMap.Tip(key2, value2) =>
      if (key == key2) VarMap.Tip(key, value)
      else join(key, VarMap.Tip(key, value), key2, this)
    case VarMap.Nil => VarMap.Tip(key, value)
  }

  /**
   * Updates the map, using the provided function to resolve conflicts if the key is already present.
   *
   * Equivalent to:
   * {{{
   *   this.get(key) match {
   *     case None => this.update(key, value)
   *     case Some(oldvalue) => this.update(key, f(oldvalue, value)
   *   }
   * }}}
   *
   * @tparam S     The supertype of values in this `LongMap`.
   * @param key    The key to update
   * @param value  The value to use if there is no conflict
   * @param f      The function used to resolve conflicts.
   * @return       The updated map.
   */
  final def updatedWithDefault[S >: T](key: Int, value: => S, f: T => S): VarMap[S] = this match {
    case VarMap.Bin(prefix, mask, left, right) =>
      if (!hasMatch(key, prefix, mask)) join(key, VarMap.Tip(key, value), prefix, this)
      else if (zero(key, mask)) VarMap.Bin(prefix, mask, left.updatedWithDefault(key, value, f), right)
      else VarMap.Bin(prefix, mask, left, right.updatedWithDefault(key, value, f))
    case VarMap.Tip(key2, value2) =>
      if (key == key2) VarMap.Tip(key, f(value2))
      else join(key, VarMap.Tip(key, value), key2, this)
    case VarMap.Nil => VarMap.Tip(key, value)
  }

  final def removed(key: Int): VarMap[T] = this match {
    case VarMap.Bin(prefix, mask, left, right) =>
      if (!hasMatch(key, prefix, mask)) this
      else if (zero(key, mask)) bin(prefix, mask, left.removed(key), right)
      else bin(prefix, mask, left, right.removed(key))
    case VarMap.Tip(key2, _) =>
      if (key == key2) VarMap.Nil
      else this
    case VarMap.Nil => VarMap.Nil
  }

  /**
   * A combined transform and filter function. Returns an `IntMap` such that
   * for each `(key, value)` mapping in this map, if `f(key, value) == None`
   * the map contains no mapping for key, and if `f(key, value)`.
   *
   * @tparam S  The type of the values in the resulting `LongMap`.
   * @param f   The transforming function.
   * @return    The modified map.
   */
  final def modifyOrRemove[S](f: (Int, T) => Option[S]): VarMap[S] = this match {
    case VarMap.Bin(prefix, mask, left, right) =>
      val newleft = left.modifyOrRemove(f)
      val newright = right.modifyOrRemove(f)
      if ((left eq newleft) && (right eq newright)) this.asInstanceOf[VarMap[S]]
      else bin(prefix, mask, newleft, newright)
    case VarMap.Tip(key, value) => f(key, value) match {
      case None =>
        VarMap.Nil
      case Some(value2) =>
        //hack to preserve sharing
        if (value.asInstanceOf[AnyRef] eq value2.asInstanceOf[AnyRef]) this.asInstanceOf[VarMap[S]]
        else VarMap.Tip(key, value2)
    }
    case VarMap.Nil =>
      VarMap.Nil
  }

  /**
   * Forms a union map with that map, using the combining function to resolve conflicts.
   *
   * @tparam S      The type of values in `that`, a supertype of values in `this`.
   * @param that    The map to form a union with.
   * @param f       The function used to resolve conflicts between two mappings.
   * @return        Union of `this` and `that`, with identical key conflicts resolved using the function `f`.
   */
  final def unionWith[S >: T](that: VarMap[S], f: (Int, S, S) => S): VarMap[S] = (this, that) match{
    case (VarMap.Bin(p1, m1, l1, r1), that@(VarMap.Bin(p2, m2, l2, r2))) =>
      if (shorter(m1, m2)) {
        if (!hasMatch(p2, p1, m1)) join(p1, this, p2, that)
        else if (zero(p2, m1)) VarMap.Bin(p1, m1, l1.unionWith(that, f), r1)
        else VarMap.Bin(p1, m1, l1, r1.unionWith(that, f))
      } else if (shorter(m2, m1)){
        if (!hasMatch(p1, p2, m2)) join(p1, this, p2, that)
        else if (zero(p1, m2)) VarMap.Bin(p2, m2, this.unionWith(l2, f), r2)
        else VarMap.Bin(p2, m2, l2, this.unionWith(r2, f))
      }
      else {
        if (p1 == p2) VarMap.Bin(p1, m1, l1.unionWith(l2,f), r1.unionWith(r2, f))
        else join(p1, this, p2, that)
      }
    case (VarMap.Tip(key, value), x) => x.updatedWithDefault[S](key, value, f(key, value, _))
    case (x, VarMap.Tip(key, value)) => x.updatedWithDefault[S](key, value, f(key, _, value))
    case (VarMap.Nil, x) => x
    case (x, VarMap.Nil) => x
  }

  /**
   * Forms the intersection of these two maps with a combining function. The
   * resulting map is a map that has only keys present in both maps and has
   * values produced from the original mappings by combining them with `f`.
   *
   * @tparam S      The type of values in `that`.
   * @tparam R      The type of values in the resulting `LongMap`.
   * @param that    The map to intersect with.
   * @param f       The combining function.
   * @return        Intersection of `this` and `that`, with values for identical keys produced by function `f`.
   */
  final def intersectionWith[S, R](that: VarMap[S], f: (Int, T, S) => R): VarMap[R] = (this, that) match {
    case (VarMap.Bin(p1, m1, l1, r1), that@VarMap.Bin(p2, m2, l2, r2)) =>
      if (shorter(m1, m2)) {
        if (!hasMatch(p2, p1, m1)) VarMap.Nil
        else if (zero(p2, m1)) l1.intersectionWith(that, f)
        else r1.intersectionWith(that, f)
      } else if (m1 == m2) bin(p1, m1, l1.intersectionWith(l2, f), r1.intersectionWith(r2, f))
      else {
        if (!hasMatch(p1, p2, m2)) VarMap.Nil
        else if (zero(p1, m2)) this.intersectionWith(l2, f)
        else this.intersectionWith(r2, f)
      }
    case (VarMap.Tip(key, value), that) => that.get(key) match {
      case None => VarMap.Nil
      case Some(value2) => VarMap.Tip(key, f(key, value, value2))
    }
    case (_, VarMap.Tip(key, value)) => this.get(key) match {
      case None => VarMap.Nil
      case Some(value2) => VarMap.Tip(key, f(key, value2, value))
    }
    case (_, _) => VarMap.Nil
  }

  /**
   * Left biased intersection. Returns the map that has all the same mappings
   * as this but only for keys which are present in the other map.
   *
   * @tparam R      The type of values in `that`.
   * @param that    The map to intersect with.
   * @return        A map with all the keys both in `this` and `that`, mapped to corresponding values from `this`.
   */
  inline def intersection[R](that: VarMap[R]): VarMap[T] =
    this.intersectionWith(that, (key: Int, value: T, value2: R) => value)

  inline def union[S >: T](that: VarMap[S]): VarMap[S] =
    this.unionWith[S](that, (key, x, _) => x)

  /**
   * The entry with the lowest key value considered in unsigned order.
   */
  @tailrec
  final def firstKey: Int = this match {
    case Bin(_, _, l, r) => l.firstKey
    case Tip(k, v) => k
    case VarMap.Nil => throw new IllegalStateException("Empty set")
  }

  /**
   * The entry with the highest key value considered in unsigned order.
   */
  @tailrec
  final def lastKey: Int = this match {
    case Bin(_, _, l, r) => r.lastKey
    case Tip(k, v) => k
    case VarMap.Nil => throw new IllegalStateException("Empty set")
  }
}
