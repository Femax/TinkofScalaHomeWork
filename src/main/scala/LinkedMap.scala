import java.util

import scala.collection.JavaConverters._

sealed trait LinkedMap[K, V] extends Traversable[(K, V)] {

  val hashMap: util.HashMap[K, V] = new util.HashMap[K, V]()
  val linkedList: util.LinkedList[(K, V)] = new util.LinkedList[(K, V)]()

  /** должен вернуть `false` если коллекция содержит хотя бы один элемент */
  override def isEmpty: Boolean = hashMap.size == 0

  /** должен вернуть `true` если коллекция содержит ключ `key` */
  def contains(key: K): Boolean = hashMap.containsKey(key)

  /** возвращает Some со значением значения, если коллекция содержит ключ `key`
    * и None если не содержит */
  def apply(key: K): Option[V] = hashMap.asScala.get(key)

  /** возвращает новый LinkedMap[K, V],
    * в котором добавлено или изменено значение для ключа `key` на `value` */
  def update(key: K, value: V): LinkedMap[K, V] = {
    val hashMap = new util.HashMap[K, V](this.hashMap)
    hashMap.put(key, value)
    LinkedMap.apply(hashMap.asScala.toSeq: _*)
  }

  /** возвращает новый LinkedMap[K, V]
    * состоящий из тех же позиций, но в обратном порядке */
  def reverse: LinkedMap[K, V] = {
    val linkedList = new util.LinkedList[(K, V)](this.linkedList)
    LinkedMap.apply(linkedList.asScala.reverse: _*)
  }

  /** создаёт новый LinkedMap, состоящий из элементов `this` и `other`
    * если какой-то ключ встречается в обеих коллекциях,
    * может быть выбрано любое значение */
  def ++(other: LinkedMap[K, V]): LinkedMap[K, V] = {
    val linkedList = new util.LinkedList[(K, V)](this.linkedList).asScala ++ other.linkedList.asScala
    LinkedMap.apply(linkedList: _*)
  }

  /** создаёт новый LinkedMap , где ко всем значениям применена заданная функция */
  def mapValues[W](f: V => W): LinkedMap[K, W] = {
    val linkedList = new util.LinkedList[(K, V)](this.linkedList).asScala
    LinkedMap.apply[K, W](linkedList.map(pair => (pair._1, f(pair._2))): _*)
  }

  /** создаёт новый LinkedMap , где ко всем значениям применена заданная функция,
    * учитывающая ключ */
  def mapWithKey[W](f: (K, V) => W): LinkedMap[K, W] = {
    val linkedList = new util.LinkedList[(K, V)](this.linkedList).asScala
    LinkedMap.apply[K, W](linkedList.map(pair => (pair._1, f(pair._1, pair._2))): _*)
  }

  /** конструирует новый LinkedMap, содеоржащий все записи текущего, кроме заданного ключа */
  def delete(key: K): LinkedMap[K, V] = {
    val hashMap = new util.HashMap[K, V](this.hashMap).asScala
    hashMap.remove(key)
    LinkedMap.apply[K, V](hashMap.toSeq: _*)
  }

  /** применяет действие `action` с побочным эффектом ко всем элементам коллекции */
  def foreach[U](action: ((K, V)) => U): Unit = {
    linkedList.asScala.foreach(pair => action(pair._1,pair._2))
  }

}

object LinkedMap {


  /** конструирует новый `LinkedMap` на основании приведённых элементов
    * каждый ключ должен присутствовать в результате только один раз
    * если в исходных данныхх ключ встречается несколько раз, может быть
    * выбрано любое из значений
    */
  def apply[K, V](kvs: (K, V)*): LinkedMap[K, V] = {
    val linkedMap = new Empty[K, V]()
    kvs.foreach(node => {
      linkedMap.hashMap.put(node._1, node._2)
      linkedMap.linkedList.add(node)
    })
    linkedMap
  }

  final case class Cons[K, V](key: K, value: V, rest: LinkedMap[K, V]) extends LinkedMap[K, V]

  final case class Empty[K, V]() extends LinkedMap[K, V]

}


