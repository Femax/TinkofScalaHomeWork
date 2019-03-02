

sealed trait LinkedMap[K, V] extends Traversable[(K, V)] {

  var map = Map[K, V]()
  var linkedList = List[(K, V)]()

  /** должен вернуть `false` если коллекция содержит хотя бы один элемент */
  override def isEmpty: Boolean = map.isEmpty

  /** должен вернуть `true` если коллекция содержит ключ `key` */
  def contains(key: K): Boolean = map.contains(key)

  /** возвращает Some со значением значения, если коллекция содержит ключ `key`
    * и None если не содержит */
  def apply(key: K): Option[V] = map.get(key)

  /** возвращает новый LinkedMap[K, V],
    * в котором добавлено или изменено значение для ключа `key` на `value` */
  def update(key: K, value: V): LinkedMap[K, V] = {
    LinkedMap.apply((this.map + (key -> value)).toSeq: _*)
  }

  /** возвращает новый LinkedMap[K, V]
    * состоящий из тех же позиций, но в обратном порядке */
  def reverse: LinkedMap[K, V] = {
    LinkedMap.apply(linkedList.reverse: _*)
  }

  /** создаёт новый LinkedMap, состоящий из элементов `this` и `other`
    * если какой-то ключ встречается в обеих коллекциях,
    * может быть выбрано любое значение */
  def ++(other: LinkedMap[K, V]): LinkedMap[K, V] = {
    LinkedMap.apply(this.linkedList ++ other: _*)
  }

  /** создаёт новый LinkedMap , где ко всем значениям применена заданная функция */
  def mapValues[W](f: V => W): LinkedMap[K, W] = {
    LinkedMap.apply[K, W](linkedList.map(pair => (pair._1, f(pair._2))): _*)
  }

  /** создаёт новый LinkedMap , где ко всем значениям применена заданная функция,
    * учитывающая ключ */
  def mapWithKey[W](f: (K, V) => W): LinkedMap[K, W] = {
    LinkedMap.apply[K, W](linkedList.map(pair => (pair._1, f(pair._1, pair._2))): _*)
  }

  /** конструирует новый LinkedMap, содеоржащий все записи текущего, кроме заданного ключа */
  def delete(key: K): LinkedMap[K, V] = {
    LinkedMap.apply[K, V]((map - key).toSeq: _*)
  }

  /** применяет действие `action` с побочным эффектом ко всем элементам коллекции */
  def foreach[U](action: ((K, V)) => U): Unit = {
    linkedList.foreach(pair => action(pair._1, pair._2))
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case obj: LinkedMap[K, V] => obj.linkedList.equals(linkedList)
    case _ => false
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
    linkedMap.map = linkedMap.map ++ Map(kvs: _*)
    linkedMap.linkedList = linkedMap.linkedList ++ List(kvs: _*)
    linkedMap
  }

  final case class Cons[K, V](key: K, value: V, rest: LinkedMap[K, V]) extends LinkedMap[K, V]

  final case class Empty[K, V]() extends LinkedMap[K, V]

}


