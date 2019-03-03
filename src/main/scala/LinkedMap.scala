import LinkedMap.{Cons, Empty}

import scala.annotation.tailrec

sealed trait LinkedMap[K, V] extends Traversable[(K, V)] {

  /** должен вернуть `false` если коллекция содержит хотя бы один элемент */
  override def isEmpty: Boolean = {
    this match {
      case Cons(_, _, _) => false
      case Empty() => true
    }
  }

  /** должен вернуть `true` если коллекция содержит ключ `key` */
  @tailrec final def contains(key: K): Boolean = this match {
    case Cons(k, _, rest) => k == key || rest.contains(key)
    case Empty() => false
  }

  /** возвращает Some со значением значения, если коллекция содержит ключ `key`
    * и None если не содержит */
  @tailrec final def apply(key: K): Option[V] = this match {
    case Cons(k, v, _) if k == key => Option(v)
    case Cons(k, _, rest) if k != key => rest.apply(key)
    case Empty() => None
  }

  /** возвращает новый LinkedMap[K, V],
    * в котором добавлено или изменено значение для ключа `key` на `value` */
  def update(key: K, value: V): LinkedMap[K, V] = Cons(key, value, this)

  /** возвращает новый LinkedMap[K, V]
    * состоящий из тех же позиций, но в обратном порядке */
  def reverse: LinkedMap[K, V] = this match {
    case Cons(k, v, rest) => reverse(Cons(k, v, rest), rest, rest.getNext)
    case Empty() => this
  }

  @tailrec private def reverse(prev: LinkedMap[K, V], current: LinkedMap[K, V], next: LinkedMap[K, V]): LinkedMap[K, V] = {
    (prev, current, next) match {
      case (Cons(k1, v1, rest1), Cons(k2, v2, _), Cons(k3, v3, rest3)) => rest3.reverse(Cons(k2, v2, Cons(k1, v1, Empty())), Cons(k3, v3, rest3), rest3)
      case (Cons(k1, v1, rest1), Cons(k2, v2, _), Empty()) => Cons(k2, v2, Cons(k1, v1, rest1))
      case _ => this
    }
  }

  //todo дублирование
  /** создаёт новый LinkedMap, состоящий из элементов `this` и `other`
    * если какой-то ключ встречается в обеих коллекциях,
    * может быть выбрано любое значение */
  def ++(other: LinkedMap[K, V]): LinkedMap[K, V] = this match {
    case Cons(k, v, rest) if rest != Empty() => Cons(k, v, rest ++ other)
    case Cons(k, v, rest) if rest == Empty() => Cons(k, v, other)
    case Empty() => this
  }

  /** создаёт новый LinkedMap , где ко всем значениям применена заданная функция */
  def mapValues[W](f: V => W): LinkedMap[K, W] = this match {
    case Cons(k, v, rest) if rest != Empty() => Cons[K, W](k, f(v), rest.mapValues(f))
    case Cons(k, v, rest) if rest == Empty() => Cons[K, W](k, f(v), Empty())
    case Empty() => Empty[K, W]()
  }

  /** создаёт новый LinkedMap , где ко всем значениям применена заданная функция,
    * учитывающая ключ */
  def mapWithKey[W](f: (K, V) => W): LinkedMap[K, W] = this match {
    case Cons(k, v, rest) if rest != Empty() => Cons[K, W](k, f(k, v), rest.mapWithKey(f))
    case Cons(k, v, rest) if rest == Empty() => Cons[K, W](k, f(k, v), Empty())
    case Empty() => Empty[K, W]()
  }

  /** конструирует новый LinkedMap, содеоржащий все записи текущего, кроме заданного ключа */
  def delete(key: K): LinkedMap[K, V] = this match {
    case Cons(k, _, rest) if k != key => rest.delete(key)
    case Cons(k, v, rest) if k == key => Cons(k, v, rest.getNext.getNext)
    case Empty() => this
  }

  /** TODO Вовзращает типизированый следующий элемент */
  private def getNext: LinkedMap[K, V] = this match {
    case Cons(k1, v1, rest) if rest != Empty() => rest
    case Cons(k1, v1, rest) if rest == Empty() => Empty()
    case Empty() => Empty()
  }

  /** применяет действие `action` с побочным эффектом ко всем элементам коллекции */
  def foreach[U](action: ((K, V)) => U): Unit = this match {
    case Cons(k, v, rest) if rest != Empty() =>
      action(k, v)
      rest.foreach(action)
    case Cons(k, v, rest) if rest == Empty() => action(k, v)
    case Empty() =>
  }

  override def toString(): String = this match {
    case Cons(k, v, rest) => s"Cons($k,$v," + rest.toString() + ")"
    case Empty() => "Empty()"
  }
}

object LinkedMap {

  /** конструирует новый `LinkedMap` на основании приведённых элементов
    * каждый ключ должен присутствовать в результате только один раз
    * если в исходных данныхх ключ встречается несколько раз, может быть
    * выбрано любое из значений
    */
  def apply[K, V](kvs: (K, V)*): LinkedMap[K, V] = {
    kvs.reverse.foldLeft(Empty[K, V](): LinkedMap[K, V]) { case (accum, (k, v)) if !accum.contains(k) => Cons(k, v, accum) }
  }

  final case class Cons[K, V](key: K, value: V, rest: LinkedMap[K, V]) extends LinkedMap[K, V]

  final case class Empty[K, V]() extends LinkedMap[K, V]

}
