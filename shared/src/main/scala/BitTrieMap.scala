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

object BitTrieLongUtils {
  inline def zero(i: Long, mask: Long) = (i & mask) == 0

  inline def mask(i: Long, mask: Long) = i & (complement(mask - 1) ^ mask)

  inline def hasMatch(key: Long, prefix: Long, m: Long) = mask(key, m) == prefix

  inline def unsignedCompare(i: Long, j: Long) = (i < j) ^ (i < 0) ^ (j < 0)

  inline def shorter(m1: Long, m2: Long) = unsignedCompare(m2, m1)

  inline def complement(i: Long) = (-1) ^ i

  inline def bits(num: Long) = 63 to 0 by -1 map (i => (num >>> i & 1) != 0)

  inline def bitString(num: Long, sep: String = "") = bits(num) map (b => if (b) "1" else "0") mkString sep

  inline def highestOneBit(j: Long) = java.lang.Long.highestOneBit(j)

  inline def branchMask(i: Long, j: Long) = highestOneBit(i ^ j)

  inline def join[T](p1: Long, t1: BitTrieMap[T], p2: Long, t2: BitTrieMap[T]): BitTrieMap[T] = {
    val m = branchMask(p1, p2)
    val p = mask(p1, m)
    if (zero(p1, m)) BitTrieMap.Bin(p, m, t1, t2)
    else BitTrieMap.Bin(p, m, t2, t1)
  }

  inline def bin[T](prefix: Long, mask: Long, left: BitTrieMap[T], right: BitTrieMap[T]): BitTrieMap[T] = (left, right) match {
    case (left, BitTrieMap.Nil) => left
    case (BitTrieMap.Nil, right) => right
    case (left, right) => BitTrieMap.Bin(prefix, mask, left, right)
  }
}

import BitTrieLongUtils._

/** A companion object for Long maps.
 *
 *  @define Coll  `LongMap`
 */
object BitTrieMap {
  inline def empty[T] : BitTrieMap[T]  = BitTrieMap.Nil

  inline def single[T](key: Long, value: T): BitTrieMap[T] = BitTrieMap.Tip(key, value)

  def apply[T](elems: (Long, T)*): BitTrieMap[T] =
    elems.foldLeft(empty[T])((x, y) => x.updated(y._1, y._2))

  def from[V](coll: IterableOnce[(Long, V)]): BitTrieMap[V] =
    var vm = BitTrieMap.empty[V]
    val it = coll.iterator
    while it.hasNext do
      val (k, v) = it.next()
      vm = vm.updated(k, v)
    vm

  def fromZip[V](keys: Array[Long], values: Array[V]): BitTrieMap[V] =
    val sz = math.min(keys.length, values.length)
    var lm = BitTrieMap.empty[V]
    var i = 0
    while (i < sz) { lm = lm.updated(keys(i), values(i)); i += 1 }
    lm

  case object Nil extends BitTrieMap[Nothing] {
    // Important! Without this equals method in place, an infinite
    // loop from Map.equals => size => pattern-match-on-Nil => equals
    // develops.  Case objects and custom equality don't mix without
    // careful handling.
    //    def equals(that : Any) = that match {
    //      case Nil => true
    //      case _ => false
    //    }
  }

  final case class Tip[+T](key: Long, value: T) extends BitTrieMap[T]{
    def withValue[S](s: S) =
      if (s.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this.asInstanceOf[BitTrieMap.Tip[S]]
      else BitTrieMap.Tip(key, s)
  }

  final case class Bin[+T](var prefix: Long, var mask: Long, var left: BitTrieMap[T @uncheckedVariance], var right: BitTrieMap[T @uncheckedVariance]) extends BitTrieMap[T] {
    def bin[S](left: BitTrieMap[S], right: BitTrieMap[S]): BitTrieMap[S] = {
      if ((this.left eq left) && (this.right eq right)) this.asInstanceOf[BitTrieMap.Bin[S]]
      else BitTrieMap.Bin[S](prefix, mask, left, right)
    }

    def joinIn(p1: Long, t1: BitTrieMap[T @uncheckedVariance], p2: Long): Unit = {
      val m = branchMask(p1, p2)
      val p = BitTrieLongUtils.mask(p1, m)
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

// Iterator over a non-empty LongMap.
abstract class BitTrieMapIterator[V, T](it: BitTrieMap[V]) extends AbstractIterator[T] {

  // Basically this uses a simple stack to emulate conversion over the tree. However
  // because we know that Longs are at least 32 bits we can have at most 32 LongMap.Bins and
  // one LongMap.Tip sitting on the tree at any poLong. Therefore we know the maximum stack
  // depth is 33 and
  var index = 0
  var buffer = new Array[AnyRef](33)

  def pop = {
    index -= 1
    buffer(index).asInstanceOf[BitTrieMap[V]]
  }

  def push(x: BitTrieMap[V]): Unit = {
    buffer(index) = x.asInstanceOf[AnyRef]
    index += 1
  }
  push(it)

  /**
   * What value do we assign to a tip?
   */
  def valueOf(tip: BitTrieMap.Tip[V]): T

  def hasNext = index != 0
  @tailrec
  final def next(): T =
    pop match {
      case BitTrieMap.Bin(_,_, t@BitTrieMap.Tip(_, _), right) => {
        push(right)
        valueOf(t)
      }
      case BitTrieMap.Bin(_, _, left, right) => {
        push(right)
        push(left)
        next()
      }
      case t@BitTrieMap.Tip(_, _) => valueOf(t)
      // This should never happen. We don't allow LongMap.Nil in subtrees of the LongMap
      // and don't return an LongMapIterator for LongMap.Nil.
      case BitTrieMap.Nil => throw new IllegalStateException("Empty maps not allowed as subtrees")
    }
}

class BitTrieMapEntryIterator[V](it: BitTrieMap[V]) extends BitTrieMapIterator[V, (Long, V)](it) {
  inline def valueOf(tip: BitTrieMap.Tip[V]) = (tip.key, tip.value)
}

class BitTrieMapValueIterator[V](it: BitTrieMap[V]) extends BitTrieMapIterator[V, V](it) {
  inline def valueOf(tip: BitTrieMap.Tip[V]) = tip.value
}

class BitTrieMapKeyIterator[V](it: BitTrieMap[V]) extends BitTrieMapIterator[V, Long](it) {
  inline def valueOf(tip: BitTrieMap.Tip[V]) = tip.key
}

import BitTrieMap._

/** Specialised immutable map structure for integer keys, based on
 *  [[https://ittc.ku.edu/~andygill/papers/IntMap98.pdf Fast Mergeable Integer Maps]]
 *  by Okasaki and Gill. Essentially a trie based on binary digits of the integers.
 *
 *  '''Note:''' This class is as of 2.8 largely superseded by HashMap.
 *
 *  @tparam T    type of the values associated with integer keys.
 *
 *  @define Coll `immutable.LongMap`
 *  @define coll immutable Longeger map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
sealed abstract class BitTrieMap[+T]  {
  inline def empty: BitTrieMap[T] = BitTrieMap.Nil

  def copy(): BitTrieMap[T] = BitTrieMap.from(this.iterator)

  /**
   * Iterator over key, value pairs of the map in unsigned order of the keys.
   *
   * @return an iterator over pairs of integer keys and corresponding values.
   */
  inline def iterator: Iterator[(Long, T)] = this match {
    case BitTrieMap.Nil => Iterator.empty
    case _ => new BitTrieMapEntryIterator(this)
  }

  final def updatedWith[T1 >: T](k: Long)(remap: Option[T] => Option[T1]): BitTrieMap[T1] =
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
  final def foreach[U](f: ((Long, T)) => U): Unit = this match {
    case BitTrieMap.Bin(_, _, left, right) => { left.foreach(f); right.foreach(f) }
    case BitTrieMap.Tip(key, value) => f((key, value))
    case BitTrieMap.Nil =>
  }

  def foreachEntry[U](f: (Long, T) => U): Unit = this match {
    case BitTrieMap.Bin(_, _, left, right) => { left.foreachEntry(f); right.foreachEntry(f) }
    case BitTrieMap.Tip(key, value) => f(key, value)
    case BitTrieMap.Nil =>
  }

  inline def keysIterator: Iterator[Long] = this match {
    case BitTrieMap.Nil => Iterator.empty
    case _ => new BitTrieMapKeyIterator(this)
  }

  /**
   * Loop over the keys of the map. The same as `keys.foreach(f)`, but may
   * be more efficient.
   *
   * @param f The loop body
   */
  final def foreachKey[U](f: Long => U): Unit = this match {
    case BitTrieMap.Bin(_, _, left, right) => { left.foreachKey(f); right.foreachKey(f) }
    case BitTrieMap.Tip(key, _) => f(key)
    case BitTrieMap.Nil =>
  }

  inline def valuesIterator: Iterator[T] = this match {
    case BitTrieMap.Nil => Iterator.empty
    case _ => new BitTrieMapValueIterator(this)
  }

  /**
   * Loop over the values of the map. The same as `values.foreach(f)`, but may
   * be more efficient.
   *
   * @param f The loop body
   */
  final def foreachValue[U](f: T => U): Unit = this match {
    case BitTrieMap.Bin(_, _, left, right) => { left.foreachValue(f); right.foreachValue(f) }
    case BitTrieMap.Tip(_, value) => f(value)
    case BitTrieMap.Nil =>
  }

  protected[this] def className = "BitTrieMap"

  inline def isEmpty = this eq BitTrieMap.Nil
  inline def nonEmpty = this ne BitTrieMap.Nil
  inline def knownSize: Long = if (isEmpty) 0 else -1
  final def filter(f: (Long, T) => Boolean): BitTrieMap[T] = this match {
    case BitTrieMap.Bin(prefix, mask, left, right) => {
      val (newleft, newright) = (left.filter(f), right.filter(f))
      if ((left eq newleft) && (right eq newright)) this
      else bin(prefix, mask, newleft, newright)
    }
    case BitTrieMap.Tip(key, value) =>
      if (f(key, value)) this
      else BitTrieMap.Nil
    case BitTrieMap.Nil => BitTrieMap.Nil
  }

  final def mapValuesNow[S](f: T => S): BitTrieMap[S] = this match {
    case b@BitTrieMap.Bin(prefix, mask, left, right) => b.bin(left.mapValuesNow(f), right.mapValuesNow(f))
    case t@BitTrieMap.Tip(key, value) => t.withValue(f(value))
    case BitTrieMap.Nil => BitTrieMap.Nil
  }

  final def transform[S](f: (Long, T) => S): BitTrieMap[S] = this match {
    case b@BitTrieMap.Bin(prefix, mask, left, right) => b.bin(left.transform(f), right.transform(f))
    case t@BitTrieMap.Tip(key, value) => t.withValue(f(key, value))
    case BitTrieMap.Nil => BitTrieMap.Nil
  }

  final def size: Long = this match {
    case BitTrieMap.Nil => 0
    case BitTrieMap.Tip(_, _) => 1
    case BitTrieMap.Bin(_, _, left, right) => left.size + right.size
  }

  @tailrec
  final def contains(key: Long): Boolean = this match {
    case BitTrieMap.Bin(prefix, mask, left, right) => if (zero(key, mask)) left.contains(key) else right.contains(key)
    case BitTrieMap.Tip(key2, value) => key == key2
    case BitTrieMap.Nil => false
  }

  @tailrec
  final def get(key: Long): Option[T] = this match {
    case BitTrieMap.Bin(prefix, mask, left, right) => if (zero(key, mask)) left.get(key) else right.get(key)
    case BitTrieMap.Tip(key2, value) => if (key == key2) Some(value) else None
    case BitTrieMap.Nil => None
  }

  @tailrec
  final def getOrElse[S >: T](key: Long, default: => S): S = this match {
    case BitTrieMap.Nil => default
    case BitTrieMap.Tip(key2, value) => if (key == key2) value else default
    case BitTrieMap.Bin(prefix, mask, left, right) =>
      if (zero(key, mask)) left.getOrElse(key, default) else right.getOrElse(key, default)
  }

  @tailrec
  final def apply(key: Long): T = this match {
    case BitTrieMap.Bin(prefix, mask, left, right) => if (zero(key, mask)) left(key) else right(key)
    case BitTrieMap.Tip(key2, value) => if (key == key2) value else throw new NoSuchElementException(key.toString)
    case BitTrieMap.Nil => throw new NoSuchElementException(key.toString)
  }

  final def updated[S >: T](key: Long, value: S): BitTrieMap[S] = this match {
    case BitTrieMap.Bin(prefix, mask, left, right) =>
      if (!hasMatch(key, prefix, mask)) join(key, BitTrieMap.Tip(key, value), prefix, this)
      else if (zero(key, mask)) BitTrieMap.Bin(prefix, mask, left.updated(key, value), right)
      else BitTrieMap.Bin(prefix, mask, left, right.updated(key, value))
    case BitTrieMap.Tip(key2, value2) =>
      if (key == key2) BitTrieMap.Tip(key, value)
      else join(key, BitTrieMap.Tip(key, value), key2, this)
    case BitTrieMap.Nil => BitTrieMap.Tip(key, value)
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
  final def updatedWithDefault[S >: T](key: Long, value: => S, f: T => S): BitTrieMap[S] = this match {
    case BitTrieMap.Bin(prefix, mask, left, right) =>
      if (!hasMatch(key, prefix, mask)) join(key, BitTrieMap.Tip(key, value), prefix, this)
      else if (zero(key, mask)) BitTrieMap.Bin(prefix, mask, left.updatedWithDefault(key, value, f), right)
      else BitTrieMap.Bin(prefix, mask, left, right.updatedWithDefault(key, value, f))
    case BitTrieMap.Tip(key2, value2) =>
      if (key == key2) BitTrieMap.Tip(key, f(value2))
      else join(key, BitTrieMap.Tip(key, value), key2, this)
    case BitTrieMap.Nil => BitTrieMap.Tip(key, value)
  }

  final def removed(key: Long): BitTrieMap[T] = this match {
    case BitTrieMap.Bin(prefix, mask, left, right) =>
      if (!hasMatch(key, prefix, mask)) this
      else if (zero(key, mask)) bin(prefix, mask, left.removed(key), right)
      else bin(prefix, mask, left, right.removed(key))
    case BitTrieMap.Tip(key2, _) =>
      if (key == key2) BitTrieMap.Nil
      else this
    case BitTrieMap.Nil => BitTrieMap.Nil
  }

  /**
   * A combined transform and filter function. Returns an `LongMap` such that
   * for each `(key, value)` mapping in this map, if `f(key, value) == None`
   * the map contains no mapping for key, and if `f(key, value)`.
   *
   * @tparam S  The type of the values in the resulting `LongMap`.
   * @param f   The transforming function.
   * @return    The modified map.
   */
  final def modifyOrRemove[S](f: (Long, T) => Option[S]): BitTrieMap[S] = this match {
    case BitTrieMap.Bin(prefix, mask, left, right) =>
      val newleft = left.modifyOrRemove(f)
      val newright = right.modifyOrRemove(f)
      if ((left eq newleft) && (right eq newright)) this.asInstanceOf[BitTrieMap[S]]
      else bin(prefix, mask, newleft, newright)
    case BitTrieMap.Tip(key, value) => f(key, value) match {
      case None =>
        BitTrieMap.Nil
      case Some(value2) =>
        //hack to preserve sharing
        if (value.asInstanceOf[AnyRef] eq value2.asInstanceOf[AnyRef]) this.asInstanceOf[BitTrieMap[S]]
        else BitTrieMap.Tip(key, value2)
    }
    case BitTrieMap.Nil =>
      BitTrieMap.Nil
  }

  /**
   * Forms a union map with that map, using the combining function to resolve conflicts.
   *
   * @tparam S      The type of values in `that`, a supertype of values in `this`.
   * @param that    The map to form a union with.
   * @param f       The function used to resolve conflicts between two mappings.
   * @return        Union of `this` and `that`, with identical key conflicts resolved using the function `f`.
   */
  final def unionWith[S >: T](that: BitTrieMap[S], f: (Long, S, S) => S): BitTrieMap[S] = (this, that) match{
    case (BitTrieMap.Bin(p1, m1, l1, r1), that@(BitTrieMap.Bin(p2, m2, l2, r2))) =>
      if (shorter(m1, m2)) {
        if (!hasMatch(p2, p1, m1)) join(p1, this, p2, that)
        else if (zero(p2, m1)) BitTrieMap.Bin(p1, m1, l1.unionWith(that, f), r1)
        else BitTrieMap.Bin(p1, m1, l1, r1.unionWith(that, f))
      } else if (shorter(m2, m1)){
        if (!hasMatch(p1, p2, m2)) join(p1, this, p2, that)
        else if (zero(p1, m2)) BitTrieMap.Bin(p2, m2, this.unionWith(l2, f), r2)
        else BitTrieMap.Bin(p2, m2, l2, this.unionWith(r2, f))
      }
      else {
        if (p1 == p2) BitTrieMap.Bin(p1, m1, l1.unionWith(l2,f), r1.unionWith(r2, f))
        else join(p1, this, p2, that)
      }
    case (BitTrieMap.Tip(key, value), x) => x.updatedWithDefault[S](key, value, f(key, value, _))
    case (x, BitTrieMap.Tip(key, value)) => x.updatedWithDefault[S](key, value, f(key, _, value))
    case (BitTrieMap.Nil, x) => x
    case (x, BitTrieMap.Nil) => x
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
  final def intersectionWith[S, R](that: BitTrieMap[S], f: (Long, T, S) => R): BitTrieMap[R] = (this, that) match {
    case (BitTrieMap.Bin(p1, m1, l1, r1), that@BitTrieMap.Bin(p2, m2, l2, r2)) =>
      if (shorter(m1, m2)) {
        if (!hasMatch(p2, p1, m1)) BitTrieMap.Nil
        else if (zero(p2, m1)) l1.intersectionWith(that, f)
        else r1.intersectionWith(that, f)
      } else if (m1 == m2) bin(p1, m1, l1.intersectionWith(l2, f), r1.intersectionWith(r2, f))
      else {
        if (!hasMatch(p1, p2, m2)) BitTrieMap.Nil
        else if (zero(p1, m2)) this.intersectionWith(l2, f)
        else this.intersectionWith(r2, f)
      }
    case (BitTrieMap.Tip(key, value), that) => that.get(key) match {
      case None => BitTrieMap.Nil
      case Some(value2) => BitTrieMap.Tip(key, f(key, value, value2))
    }
    case (_, BitTrieMap.Tip(key, value)) => this.get(key) match {
      case None => BitTrieMap.Nil
      case Some(value2) => BitTrieMap.Tip(key, f(key, value2, value))
    }
    case (_, _) => BitTrieMap.Nil
  }

  def subtractWith[S >: T](p: (S, S) => Option[S])(m2: BitTrieMap[S]): BitTrieMap[S] =
    var n = BitTrieMap.empty[S]
    if this.isEmpty then n
    else
      this.foreachEntry((k, v) =>
        m2.get(k) match
          case None => n = n.updated(k, v)
          case Some(v_) =>
            p(v, v_) match
              case Some(r) => n = n.updated(k, r)
              case None => ()
      )
      n


  /**
   * Left biased intersection. Returns the map that has all the same mappings
   * as this but only for keys which are present in the other map.
   *
   * @tparam R      The type of values in `that`.
   * @param that    The map to intersect with.
   * @return        A map with all the keys both in `this` and `that`, mapped to corresponding values from `this`.
   */
  inline def intersection[R](that: BitTrieMap[R]): BitTrieMap[T] =
    this.intersectionWith(that, (key: Long, value: T, value2: R) => value)

  inline def union[S >: T](that: BitTrieMap[S]): BitTrieMap[S] =
    this.unionWith[S](that, (key, x, _) => x)

  inline def subtract[S >: T](that: BitTrieMap[S]): BitTrieMap[S] =
    if this.isEmpty then BitTrieMap.empty
    else this.filter((k, _) => !that.contains(k))

  /**
   * The entry with the lowest key value considered in unsigned order.
   */
  @tailrec
  final def firstKey: Long = this match {
    case Bin(_, _, l, r) => l.firstKey
    case Tip(k, v) => k
    case BitTrieMap.Nil => throw new IllegalStateException("Empty set")
  }

  /**
   * The entry with the highest key value considered in unsigned order.
   */
  @tailrec
  final def lastKey: Long = this match {
    case Bin(_, _, l, r) => r.lastKey
    case Tip(k, v) => k
    case BitTrieMap.Nil => throw new IllegalStateException("Empty set")
  }
}


