package be.adamv.cz2


trait AbstractVarMap[V, CC[_], C <: AbstractVarMap[V, CC, C]]:
  def union(m2: C): C
  def unionWith(op: (V, V) => V)(m2: C): C
  def intersection(m2: C): C
  def intersectionWith(op: (V, V) => V)(m2: C): C
  def subtract(m2: C): C
  def subtractWith(p: (V, V) => Option[V])(m2: C): C

  def modifyOrRemove[V2](f: (Long, V) => Option[V2]): CC[V2]

  def updateWithDefault(key: Long, value: => V, f: V => V): Unit
  def updateWith(key: Long)(remappingFunction: (Option[V]) => Option[V]): Option[V]
  def remove(key: Long): Option[V]


export be.adamv.cz2.OpenHashTable as VarMap
