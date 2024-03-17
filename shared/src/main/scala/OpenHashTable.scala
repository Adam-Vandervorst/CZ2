package be.adamv.cz2
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

import scala.collection.generic.DefaultSerializationProxy
import scala.language.implicitConversions

/** This class implements mutable maps with `Long` keys based on a hash table with open addressing.
 *
 *  Basic map operations on single entries, including `contains` and `get`,
 *  are typically substantially faster with `LongMap` than [[HashMap]].  Methods
 *  that act on the whole map,  including `foreach` and `map` are not in
 *  general expected to be faster than with a generic map, save for those
 *  that take particular advantage of the internal structure of the map:
 *  `foreachKey`, `foreachValue`, `mapValuesNow`, and `transformValues`.
 *
 *  Maps with open addressing may become less efficient at lookup after
 *  repeated addition/removal of elements.  Although `LongMap` makes a
 *  decent attempt to remain efficient regardless,  calling `repack`
 *  on a map that will no longer have elements removed but will be
 *  used heavily may save both time and storage space.
 *
 *  This map is not intended to contain more than 2^29 entries (approximately
 *  500 million).  The maximum capacity is 2^30, but performance will degrade
 *  rapidly as 2^30 is approached.
 *
 */
final class OpenHashTable[V](defaultEntry: Long => V, initialBufferSize: Int, initBlank: Boolean)
    extends AbstractVarMap[V, OpenHashTable, OpenHashTable[V]]
    with Serializable {
  import OpenHashTable._

  inline def filter(pred: (Long, V) => Boolean): OpenHashTable[V] =
    val n = OpenHashTable.empty[V]
    this.foreachEntry((k, v) => if pred(k, v) then n.update(k, v))
    n

  inline def foldLeft[B](z: B)(op: (B, (Long, V)) => B): B =
    var r = z
    this.foreachEntry((k, v) => r = op(r, (k, v)))
    r

  inline def union(m2: OpenHashTable[V]): OpenHashTable[V] =
    val n = this.clone()
    m2.foreachEntry(n.update)
    n

  def unionWith(op: (V, V) => V)(m2: OpenHashTable[V]): OpenHashTable[V] =
    val n = this.clone()
    m2.foreachEntry((k, v) =>
      var required = true
      val v_ = n.getOrElseUpdate(k, {
        required = false; v
      })
      if required then n.update(k, op(v, v_))
    )
    n


  def intersection(m2: OpenHashTable[V]): OpenHashTable[V] =
    val n = this.clone()
    this.foreachKey(k => if !m2.contains(k) then n.remove(k))
    n

  def intersectionWith(op: (V, V) => V)(m2: OpenHashTable[V]): OpenHashTable[V] =
    val n = OpenHashTable.empty[V]
    this.foreachEntry((k, v) =>
      m2.get(k) match
        case None => ()
        case Some(v_) => n.update(k, op(v, v_))
    )
    n

  def subtract(m2: OpenHashTable[V]): OpenHashTable[V] =
    if this.isEmpty then OpenHashTable.empty
    else
      this.filter((k, _) => !m2.contains(k))

  def subtractWith(p: (V, V) => Option[V])(m2: OpenHashTable[V]): OpenHashTable[V] =
    val n = OpenHashTable.empty[V]
    if this.isEmpty then n
    else
      this.foreachEntry((k, v) =>
        m2.get(k) match
          case None => n.update(k, v)
          case Some(v_) =>
            p(v, v_) match
              case Some(r) => n.update(k, r)
              case None => ()
      )
      n

  def this() = this(OpenHashTable.exceptionDefault, 16, true)

  // TODO: clear() with an optimization more tailored for efficiency.
  protected def fromSpecific(coll: scala.collection.IterableOnce[(Long, V)]): OpenHashTable[V] = {
    //TODO should this be the default implementation of this method in StrictOptimizedIterableOps?
    val b = newSpecificBuilder
    b.sizeHint(coll)
    b.addAll(coll)
    b.result()
  }
  protected def newSpecificBuilder: collection.mutable.Builder[(Long, V),OpenHashTable[V]] = new collection.mutable.GrowableBuilder(OpenHashTable.empty[V].asInstanceOf[scala.collection.mutable.Growable[(Long, V)] & be.adamv.cz2.OpenHashTable[V]])

  /** Creates a new `LongMap` that returns default values according to a supplied key-value mapping. */
  def this(defaultEntry: Long => V) = this(defaultEntry, 16, true)

  /** Creates a new `LongMap` with an initial buffer of specified size.
   *
   *  A LongMap can typically contain half as many elements as its buffer size
   *  before it requires resizing.
   */
  def this(initialBufferSize: Int) = this(OpenHashTable.exceptionDefault, initialBufferSize, true)

  /** Creates a new `LongMap` with specified default values and initial buffer size. */
  def this(defaultEntry: Long => V,  initialBufferSize: Int) = this(defaultEntry,  initialBufferSize,  true)

  private[this] var mask = 0
  private[this] var extraKeys: Int = 0
  private[this] var zeroValue: AnyRef = null
  private[this] var minValue: AnyRef = null
  private[this] var _size = 0
  private[this] var _vacant = 0
  private[this] var _keys: Array[Long] = null
  private[this] var _values: Array[AnyRef] = null

  if (initBlank) defaultInitialize(initialBufferSize)

  private[this] def defaultInitialize(n: Int) = {
    mask =
      if (n<0) 0x7
      else (((1 << (32 - java.lang.Integer.numberOfLeadingZeros(n-1))) - 1) & 0x3FFFFFFF) | 0x7
    _keys = new Array[Long](mask+1)
    _values = new Array[AnyRef](mask+1)
  }

  private def initializeTo(
                                        m: Int, ek: Int, zv: AnyRef, mv: AnyRef, sz: Int, vc: Int, kz: Array[Long], vz: Array[AnyRef]
                                      ): Unit = {
    mask = m; extraKeys = ek; zeroValue = zv; minValue = mv; _size = sz; _vacant = vc; _keys = kz; _values = vz
  }

  def size: Int = _size + (extraKeys+1)/2
  def knownSize: Int = size
  def isEmpty: Boolean = size == 0
  def nonEmpty: Boolean = size != 0
  def empty: OpenHashTable[V] = new OpenHashTable()

  private def imbalanced: Boolean =
    (_size + _vacant) > 0.5*mask || _vacant > _size

  private def toIndex(k: Long): Int = {
    // Part of the MurmurHash3 32 bit finalizer
    val h = ((k ^ (k >>> 32)) & 0xFFFFFFFFL).toInt
    val x = (h ^ (h >>> 16)) * 0x85EBCA6B
    (x ^ (x >>> 13)) & mask
  }

  private def seekEmpty(k: Long): Int = {
    var e = toIndex(k)
    var x = 0
    while (_keys(e) != 0) { x += 1; e = (e + 2*(x+1)*x - 3) & mask }
    e
  }

  private def seekEntry(k: Long): Int = {
    var e = toIndex(k)
    var x = 0
    var q = 0L
    while ({ q = _keys(e); if (q==k) return e; q != 0}) { x += 1; e = (e + 2*(x+1)*x - 3) & mask }
    e | MissingBit
  }

  private def seekEntryOrOpen(k: Long): Int = {
    var e = toIndex(k)
    var x = 0
    var q = 0L
    while ({ q = _keys(e); if (q==k) return e; q+q != 0}) {
      x += 1
      e = (e + 2*(x+1)*x - 3) & mask
    }
    if (q == 0) return e | MissingBit
    val o = e | MissVacant
    while ({ q = _keys(e); if (q==k) return e; q != 0}) {
      x += 1
      e = (e + 2*(x+1)*x - 3) & mask
    }
    o
  }

  def contains(key: Long): Boolean = {
    if (key == -key) (((key>>>63).toInt+1) & extraKeys) != 0
    else seekEntry(key) >= 0
  }

  def get(key: Long): Option[V] = {
    if (key == -key) {
      if ((((key>>>63).toInt+1) & extraKeys) == 0) None
      else if (key == 0) Some(zeroValue.asInstanceOf[V])
      else Some(minValue.asInstanceOf[V])
    }
    else {
      val i = seekEntry(key)
      if (i < 0) None else Some(_values(i).asInstanceOf[V])
    }
  }

  def getOrElse[V1 >: V](key: Long, default: => V1): V1 = {
    if (key == -key) {
      if ((((key>>>63).toInt+1) & extraKeys) == 0) default
      else if (key == 0) zeroValue.asInstanceOf[V1]
      else minValue.asInstanceOf[V1]
    }
    else {
      val i = seekEntry(key)
      if (i < 0) default else _values(i).asInstanceOf[V1]
    }
  }

  def getOrElseUpdate(key: Long, defaultValue: => V): V = {
    if (key == -key) {
      val kbits = (key>>>63).toInt + 1
      if ((kbits & extraKeys) == 0) {
        val value = defaultValue
        extraKeys |= kbits
        if (key == 0) zeroValue = value.asInstanceOf[AnyRef]
        else minValue = value.asInstanceOf[AnyRef]
        value
      }
      else if (key == 0) zeroValue.asInstanceOf[V]
      else minValue.asInstanceOf[V]
    }
    else {
      var i = seekEntryOrOpen(key)
      if (i < 0) {
        // It is possible that the default value computation was side-effecting
        // Our hash table may have resized or even contain what we want now
        // (but if it does, we'll replace it)
        val value = {
          val ok = _keys
          val ans = defaultValue
          if (ok ne _keys) {
            i = seekEntryOrOpen(key)
            if (i >= 0) _size -= 1
          }
          ans
        }
        _size += 1
        val j = i & IndexMask
        _keys(j) = key
        _values(j) = value.asInstanceOf[AnyRef]
        if ((i & VacantBit) != 0) _vacant -= 1
        else if (imbalanced) repack()
        value
      }
      else _values(i).asInstanceOf[V]
    }
  }

  /** Retrieves the value associated with a key, or the default for that type if none exists
   *  (null for AnyRef, 0 for floats and integers).
   *
   *  Note: this is the fastest way to retrieve a value that may or
   *  may not exist, if the default null/zero is acceptable.  For key/value
   *  pairs that do exist,  `apply` (i.e. `map(key)`) is equally fast.
   */
  def getOrNull(key: Long): V = {
    if (key == -key) {
      if ((((key>>>63).toInt+1) & extraKeys) == 0) null.asInstanceOf[V]
      else if (key == 0) zeroValue.asInstanceOf[V]
      else minValue.asInstanceOf[V]
    }
    else {
      val i = seekEntry(key)
      if (i < 0) null.asInstanceOf[V] else _values(i).asInstanceOf[V]
    }
  }

  /** Retrieves the value associated with a key.
   *  If the key does not exist in the map, the `defaultEntry` for that key
   *  will be returned instead.
   */
  def apply(key: Long): V = {
    if (key == -key) {
      if ((((key>>>63).toInt+1) & extraKeys) == 0) defaultEntry(key)
      else if (key == 0) zeroValue.asInstanceOf[V]
      else minValue.asInstanceOf[V]
    }
    else {
      val i = seekEntry(key)
      if (i < 0) defaultEntry(key) else _values(i).asInstanceOf[V]
    }
  }

  /** The user-supplied default value for the key.  Throws an exception
   *  if no other default behavior was specified.
   */
  def default(key: Long) = defaultEntry(key)

  private def repack(newMask: Int): Unit = {
    val ok = _keys
    val ov = _values
    mask = newMask
    _keys = new Array[Long](mask+1)
    _values = new Array[AnyRef](mask+1)
    _vacant = 0
    var i = 0
    while (i < ok.length) {
      val k = ok(i)
      if (k != -k) {
        val j = seekEmpty(k)
        _keys(j) = k
        _values(j) = ov(i)
      }
      i += 1
    }
  }

  /** Repacks the contents of this `LongMap` for maximum efficiency of lookup.
   *
   *  For maps that undergo a complex creation process with both addition and
   *  removal of keys, and then are used heavily with no further removal of
   *  elements, calling `repack` after the end of the creation can result in
   *  improved performance.  Repacking takes time proportional to the number
   *  of entries in the map.
   */
  def repack(): Unit = {
    var m = mask
    if (_size + _vacant >= 0.5*mask && !(_vacant > 0.2*mask)) m = ((m << 1) + 1) & IndexMask
    while (m > 8 && 8*_size < m) m = m >>> 1
    repack(m)
  }

  def put(key: Long, value: V): Option[V] = {
    if (key == -key) {
      if (key == 0) {
        val ans = if ((extraKeys&1) == 1) Some(zeroValue.asInstanceOf[V]) else None
        zeroValue = value.asInstanceOf[AnyRef]
        extraKeys |= 1
        ans
      }
      else {
        val ans = if ((extraKeys&2) == 1) Some(minValue.asInstanceOf[V]) else None
        minValue = value.asInstanceOf[AnyRef]
        extraKeys |= 2
        ans
      }
    }
    else {
      val i = seekEntryOrOpen(key)
      if (i < 0) {
        val j = i & IndexMask
        _keys(j) = key
        _values(j) = value.asInstanceOf[AnyRef]
        _size += 1
        if ((i & VacantBit) != 0) _vacant -= 1
        else if (imbalanced) repack()
        None
      }
      else {
        val ans = Some(_values(i).asInstanceOf[V])
        _keys(i) = key
        _values(i) = value.asInstanceOf[AnyRef]
        ans
      }
    }
  }

  /** Updates the map to include a new key-value pair.
   *
   *  This is the fastest way to add an entry to a `LongMap`.
   */
  def update(key: Long, value: V): Unit = {
    if (key == -key) {
      if (key == 0) {
        zeroValue = value.asInstanceOf[AnyRef]
        extraKeys |= 1
      }
      else {
        minValue = value.asInstanceOf[AnyRef]
        extraKeys |= 2
      }
    }
    else {
      val i = seekEntryOrOpen(key)
      if (i < 0) {
        val j = i & IndexMask
        _keys(j) = key
        _values(j) = value.asInstanceOf[AnyRef]
        _size += 1
        if ((i & VacantBit) != 0) _vacant -= 1
        else if (imbalanced) repack()
      }
      else {
        _keys(i) = key
        _values(i) = value.asInstanceOf[AnyRef]
      }
    }
  }

  /** Adds a new key/value pair to this map and returns the map. */
  @deprecated("Use `addOne` or `update` instead; infix operations with an operand of multiple args will be deprecated", "2.13.3")
  def +=(key: Long, value: V): this.type = { update(key, value); this }

  /** Adds a new key/value pair to this map and returns the map. */
  @inline final def addOne(key: Long, value: V): this.type = { update(key, value); this }

  @inline final def addOne(kv: (Long, V)): this.type = { update(kv._1, kv._2); this }

  inline def remove(key: Long): Option[V] =
    val r = this.get(key)
    if r.nonEmpty then this.subtractOne(key)
    r

  def updateWithDefault(key: Long, value: => V, f: V => V): Unit =
    this.update(key, this.get(key) match
      case None => value
      case Some(v) => f(v)
    )

  def updateWith(key: Long)(remappingFunction: (Option[V]) => Option[V]): Option[V] =
    val i = this.get(key)
    val o = remappingFunction(i)
    o match
      case Some(value) => this.update(key, value)
      case None => if i.nonEmpty then this.subtractOne(key)
    o

  def subtractOne(key: Long): this.type = {
    if (key == -key) {
      if (key == 0L) {
        extraKeys &= 0x2
        zeroValue = null
      }
      else {
        extraKeys &= 0x1
        minValue = null
      }
    }
    else {
      val i = seekEntry(key)
      if (i >= 0) {
        _size -= 1
        _vacant += 1
        _keys(i) = Long.MinValue
        _values(i) = null
      }
    }
    this
  }

  def iterator: Iterator[(Long, V)] = new collection.AbstractIterator[(Long, V)] {
    private[this] val kz = _keys
    private[this] val vz = _values

    private[this] var nextPair: (Long, V) =
      if (extraKeys==0) null
      else if ((extraKeys&1)==1) (0L, zeroValue.asInstanceOf[V])
      else (Long.MinValue, minValue.asInstanceOf[V])

    private[this] var anotherPair: (Long, V) =
      if (extraKeys==3) (Long.MinValue, minValue.asInstanceOf[V])
      else null

    private[this] var index = 0

    def hasNext: Boolean = nextPair != null || (index < kz.length && {
      var q = kz(index)
      while (q == -q) {
        index += 1
        if (index >= kz.length) return false
        q = kz(index)
      }
      nextPair = (kz(index), vz(index).asInstanceOf[V])
      index += 1
      true
    })
    def next() = {
      if (nextPair == null && !hasNext) throw new NoSuchElementException("next")
      val ans = nextPair
      if (anotherPair != null) {
        nextPair = anotherPair
        anotherPair = null
      }
      else nextPair = null
      ans
    }
  }

  // TODO PERF these for efficiency. See immutable.LongMap for how to organize the code.
  def keysIterator: Iterator[Long] = iterator.map(_._1)
  def valuesIterator: Iterator[V] = iterator.map(_._2)

  def foreach[U](f: ((Long,V)) => U): Unit = {
    if ((extraKeys & 1) == 1) f((0L, zeroValue.asInstanceOf[V]))
    if ((extraKeys & 2) == 2) f((Long.MinValue, minValue.asInstanceOf[V]))
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        f((k, _values(i).asInstanceOf[V]))
      }
      i += 1
    }
  }

  def foreachEntry[U](f: (Long,V) => U): Unit = {
    if ((extraKeys & 1) == 1) f(0L, zeroValue.asInstanceOf[V])
    if ((extraKeys & 2) == 2) f(Long.MinValue, minValue.asInstanceOf[V])
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        f(k, _values(i).asInstanceOf[V])
      }
      i += 1
    }
  }

  override def clone(): OpenHashTable[V] = {
    val kz = java.util.Arrays.copyOf(_keys, _keys.length)
    val vz = java.util.Arrays.copyOf(_values,  _values.length)
    val lm = new OpenHashTable[V](defaultEntry, 1, false)
    lm.initializeTo(mask, extraKeys, zeroValue, minValue, _size, _vacant, kz,  vz)
    lm
  }

  /** Applies a function to all keys of this map. */
  def foreachKey[A](f: Long => A): Unit = {
    if ((extraKeys & 1) == 1) f(0L)
    if ((extraKeys & 2) == 2) f(Long.MinValue)
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        f(k)
      }
      i += 1
    }
  }

  /** Applies a function to all values of this map. */
  def foreachValue[A](f: V => A): Unit = {
    if ((extraKeys & 1) == 1) f(zeroValue.asInstanceOf[V])
    if ((extraKeys & 2) == 2) f(minValue.asInstanceOf[V])
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        f(_values(i).asInstanceOf[V])
      }
      i += 1
    }
  }

  /** Creates a new `LongMap` with different values.
   *  Unlike `mapValues`, this method generates a new
   *  collection immediately.
   */
  def mapValuesNow[V1](f: V => V1): OpenHashTable[V1] = {
    val zv = if ((extraKeys & 1) == 1) f(zeroValue.asInstanceOf[V]).asInstanceOf[AnyRef] else null
    val mv = if ((extraKeys & 2) == 2) f(minValue.asInstanceOf[V]).asInstanceOf[AnyRef] else null
    val lm = new OpenHashTable[V1](OpenHashTable.exceptionDefault,  1,  false)
    val kz = java.util.Arrays.copyOf(_keys, _keys.length)
    val vz = new Array[AnyRef](_values.length)
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        vz(i) = f(_values(i).asInstanceOf[V]).asInstanceOf[AnyRef]
      }
      i += 1
    }
    lm.initializeTo(mask, extraKeys, zv, mv, _size, _vacant, kz, vz)
    lm
  }

  /** Applies a transformation function to all values stored in this map.
   *  Note: the default, if any,  is not transformed.
   */
  @deprecated("Use transformValuesInPlace instead of transformValues", "2.13.0")
  @`inline` final def transformValues(f: V => V): this.type = transformValuesInPlace(f)

  /** Applies a transformation function to all values stored in this map.
   *  Note: the default, if any,  is not transformed.
   */
  def transformValuesInPlace(f: V => V): this.type = {
    if ((extraKeys & 1) == 1) zeroValue = f(zeroValue.asInstanceOf[V]).asInstanceOf[AnyRef]
    if ((extraKeys & 2) == 2) minValue = f(minValue.asInstanceOf[V]).asInstanceOf[AnyRef]
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        _values(i) = f(_values(i).asInstanceOf[V]).asInstanceOf[AnyRef]
      }
      i += 1
    }
    this
  }

  def map[V2](f: ((Long, V)) => (Long, V2)): OpenHashTable[V2] =
    val n = OpenHashTable.empty[V2]
    this.foreachEntry((k, v) => n.addOne(f(k, v)))
    n

  def flatMap[V2](f: ((Long, V)) => IterableOnce[(Long, V2)]): OpenHashTable[V2] =
    OpenHashTable.from(this.iterator.flatMap(f))


  def modifyOrRemove[V2](f: (Long, V) => Option[V2]): OpenHashTable[V2] =
    val n = OpenHashTable.empty[V2]
    this.foreachEntry((k, v) => f(k, v) match
      case Some(value) => n.update(k, value)
      case None => ())
    n

  def collect[V2](pf: PartialFunction[(Long, V), (Long, V2)]): OpenHashTable[V2] =
    val b = OpenHashTable.empty[V2]
    val marker = scala.runtime.Statics.pfMarker
    val it = iterator
    while (it.hasNext) {
      val elem = it.next()
      val v = pf.applyOrElse(elem, ((x: (Long, V)) => marker).asInstanceOf[Function[(Long, V), (Long, V2)]])
      if (marker ne v.asInstanceOf[AnyRef]) b.addOne(v)
    }
    b

  override def equals(obj: Any): Boolean = obj match
    case that: OpenHashTable[V] if this.size == that.size =>
      if (extraKeys & 1) == 1 then
        if !that.get(0L).contains(zeroValue.asInstanceOf[V]) then return false
      if (extraKeys & 2) == 2 then
        if !that.get(Long.MinValue).contains(minValue.asInstanceOf[V]) then return false

      var i, j = 0
      while (i < _keys.length & j < _size) {
        val k = _keys(i)
        if (k != -k) {
          j += 1
          if !that.get(k).contains(_values(i).asInstanceOf[V]) then return false
        }
        i += 1
      }
      true
    case _ => false

  override def toString: String =
    this.iterator.mkString("OpenHashTable(", ",", ")")

  protected[this] def writeReplace(): AnyRef = new DefaultSerializationProxy(OpenHashTable.toFactory[V](OpenHashTable), this.iterator.toArray)

  protected[this] def className = "LongMap"
}

object OpenHashTable {
  private final val IndexMask  = 0x3FFFFFFF
  private final val MissingBit = 0x80000000
  private final val VacantBit  = 0x40000000
  private final val MissVacant = 0xC0000000

  private val exceptionDefault: Long => Nothing = (k: Long) => throw new NoSuchElementException(k.toString)

  /** A builder for instances of `LongMap`.
   *
   *  This builder can be reused to create multiple instances.
   */
  final class LongMapBuilder[V] extends collection.mutable.ReusableBuilder[(Long, V), OpenHashTable[V]] {
    private var elems: OpenHashTable[V] = new OpenHashTable[V]
    def addOne(entry: (Long, V)): this.type = {
      elems.addOne(entry)
      this
    }
    def clear(): Unit = elems = new OpenHashTable[V]
    def result(): OpenHashTable[V] = elems
    override def knownSize: Int = elems.knownSize
  }

  /** Creates a new `LongMap` with zero or more key/value pairs. */
  def apply[V](elems: (Long, V)*): OpenHashTable[V] = buildFromIterableOnce(elems)

  inline def single[V](k: Long, v: V): OpenHashTable[V] =
    val m = OpenHashTable.empty[V]
    m.update(k, v)
    m

  private def buildFromIterableOnce[V](elems: IterableOnce[(Long, V)]): OpenHashTable[V] = {
    var sz = elems.knownSize
    if(sz < 0) sz = 4
    val lm = new OpenHashTable[V](sz * 2)
    elems.iterator.foreach{ case (k,v) => lm(k) = v }
    if (lm.size < (sz>>3)) lm.repack()
    lm
  }

  /** Creates a new empty `LongMap`. */
  def empty[V]: OpenHashTable[V] = new OpenHashTable[V]

  /** Creates a new empty `LongMap` with the supplied default */
  def withDefault[V](default: Long => V): OpenHashTable[V] = new OpenHashTable[V](default)

  /** Creates a new `LongMap` from an existing source collection. A source collection
   * which is already a `LongMap` gets cloned.
   *
   * @param source Source collection
   * @tparam A the type of the collection’s elements
   * @return a new `LongMap` with the elements of `source`
   */
  def from[V](source: IterableOnce[(Long, V)]): OpenHashTable[V] = source match {
    case _ => buildFromIterableOnce(source)
  }

  def newBuilder[V]: collection.mutable.ReusableBuilder[(Long, V), OpenHashTable[V]] = new LongMapBuilder[V]

  /** Creates a new `LongMap` from arrays of keys and values.
   *  Equivalent to but more efficient than `LongMap((keys zip values): _*)`.
   */
  def fromZip[V](keys: Array[Long], values: Array[V]): OpenHashTable[V] = {
    val sz = math.min(keys.length, values.length)
    val lm = new OpenHashTable[V](sz * 2)
    var i = 0
    while (i < sz) { lm(keys(i)) = values(i); i += 1 }
    if (lm.size < (sz>>3)) lm.repack()
    lm
  }

  /** Creates a new `LongMap` from keys and values.
   *  Equivalent to but more efficient than `LongMap((keys zip values): _*)`.
   */
  def fromZip[V](keys: scala.collection.Iterable[Long], values: scala.collection.Iterable[V]): OpenHashTable[V] = {
    val sz = math.min(keys.size, values.size)
    val lm = new OpenHashTable[V](sz * 2)
    val ki = keys.iterator
    val vi = values.iterator
    while (ki.hasNext && vi.hasNext) lm(ki.next()) = vi.next()
    if (lm.size < (sz >> 3)) lm.repack()
    lm
  }

  implicit def toFactory[V](dummy: OpenHashTable.type): collection.Factory[(Long, V), OpenHashTable[V]] = ToFactory.asInstanceOf[collection.Factory[(Long, V), OpenHashTable[V]]]

  @SerialVersionUID(3L)
  private[this] object ToFactory extends collection.Factory[(Long, AnyRef), OpenHashTable[AnyRef]] with Serializable {
    def fromSpecific(it: IterableOnce[(Long, AnyRef)]): OpenHashTable[AnyRef] = OpenHashTable.from[AnyRef](it)
    def newBuilder: collection.mutable.Builder[(Long, AnyRef), OpenHashTable[AnyRef]] = OpenHashTable.newBuilder[AnyRef]
  }
//
//  implicit def toBuildFrom[V](factory: OpenHashTable.type): BuildFrom[Any, (Long, V), OpenHashTable[V]] = ToBuildFrom.asInstanceOf[BuildFrom[Any, (Long, V), OpenHashTable[V]]]
//  private object ToBuildFrom extends BuildFrom[Any, (Long, AnyRef), OpenHashTable[AnyRef]] {
//    def fromSpecific(from: Any)(it: IterableOnce[(Long, AnyRef)]) = OpenHashTable.from(it)
//    def newBuilder(from: Any) = OpenHashTable.newBuilder[AnyRef]
//  }
//
//  implicit def iterableFactory[V]: Factory[(Long, V), OpenHashTable[V]] = toFactory(this)
//  implicit def buildFromLongMap[V]: BuildFrom[OpenHashTable[_], (Long, V), OpenHashTable[V]] = toBuildFrom(this)
}
