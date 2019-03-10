import LinkedMap.{Cons, Empty}

import scala.annotation.tailrec

sealed trait LinkedMap[K, V] extends Traversable[(K, V)] {

  /** должен вернуть `false` если коллекция содержит хотя бы один элемент */
  override def isEmpty: Boolean = this match {
    case Cons(_, _, _) => false
    case Empty() => true
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
  def update(key: K, value: V): LinkedMap[K, V] = this match {
    case Cons(k, v, _) => update(key, value, Empty(), updated = false)
    case Empty() => Cons(key, value, Empty())
  }

  @tailrec private def update(key: K, value: V, accumm: LinkedMap[K, V], updated: Boolean): LinkedMap[K, V] = this match {
    case Cons(k, v, rest) if k != key => rest.update(key, value, Cons(k, v, accumm), updated)
    case Cons(k, _, rest) if k == key => rest.update(key, value, Cons(key, value, accumm.getNext), updated = true)
    case Empty() if !updated => Cons(key, value, accumm).reverse
    case Empty() if updated => accumm.reverse
  }

  /** возвращает новый LinkedMap[K, V]
    * состоящий из тех же позиций, но в обратном порядке */
  final def reverse: LinkedMap[K, V] = this match {
    case Cons(k, v, _) => reverse(Empty())
    case Empty() => this
  }

  @tailrec private def reverse(accumm: LinkedMap[K, V]): LinkedMap[K, V] = this match {
    case Cons(k, v, rest) => rest.reverse(Cons(k, v, accumm))
    case Empty() => accumm
  }

  /** создаёт новый LinkedMap, состоящий из элементов `this` и `other`
    * если какой-то ключ встречается в обеих коллекциях,
    * может быть выбрано любое значение */
  final def ++(other: LinkedMap[K, V]): LinkedMap[K, V] = this match {
    case Cons(k, v, rest) => ++(other, this.toTailWithReverse())
    case Empty() => other
  }

  def ++(other: LinkedMap[K, V], accum: LinkedMap[K, V]): LinkedMap[K, V] = other match {
    case Cons(k, v, rest) if !accum.contains(k) => {
      rest.++(rest, Cons(k, v, accum))
    }
    case Cons(k, v, rest) if accum.contains(k) => {
      rest.++(rest, accum)
    }
    case Empty() => accum.reverse
  }

  @tailrec private def toTailWithReverse(accum: LinkedMap[K, V] = Empty()): LinkedMap[K, V] = this match {
    case Cons(k, v, rest) => rest.toTailWithReverse(Cons(k, v, accum))
    case Empty() => accum
  }

  /** создаёт новый LinkedMap , где ко всем значениям применена заданная функция */
  def mapValues[W](f: V => W): LinkedMap[K, W] = this match {
    case Cons(k, v, rest) => mapValues(f, Empty())
    case Empty() => Empty[K, W]()
  }

  @tailrec private def mapValues[W](f: V => W, accum: LinkedMap[K, W]): LinkedMap[K, W] = this match {
    case Cons(k, v, rest) => rest.mapValues(f, Cons(k, f(v), accum))
    case Empty() => accum.reverse
  }

  /** создаёт новый LinkedMap , где ко всем значениям применена заданная функция,
    * учитывающая ключ */
  def mapWithKey[W](f: (K, V) => W): LinkedMap[K, W] = this match {
    case Cons(k, v, rest) => mapWithKey(f, Empty())
    case Empty() => Empty[K, W]()
  }

  @tailrec private def mapWithKey[W](f: (K, V) => W, accum: LinkedMap[K, W]): LinkedMap[K, W] = this match {
    case Cons(k, v, rest) => rest.mapWithKey(f, Cons(k, f(k, v), accum))
    case Empty() => accum.reverse
  }

  /** конструирует новый LinkedMap, содеоржащий все записи текущего, кроме заданного ключа */
  def delete(key: K): LinkedMap[K, V] = this match {
    case Cons(k, v, rest) if k != key => delete(key, rest, Empty())
    case Cons(k, _, rest) if k == key => rest
    case Empty() => this
  }

  @tailrec private def delete(key: K, next: LinkedMap[K, V], accum: LinkedMap[K, V]): LinkedMap[K, V] = (this, next) match {
    case (Cons(k1, v1, rest1), Cons(k2, v2, rest2)) if k1 != key => rest1.delete(key, rest2, Cons(k1, v1, accum))
    case (Cons(k1, v1, _), Cons(k2, v2, rest2)) if k1 == key => rest2.delete(key, rest2.getNext, Cons(k2, v2, accum))
    case (Cons(k, v, rest), Empty()) if k == key => accum.reverse
    case (Cons(k, v, rest), Empty()) if k != key => Cons(k, v, accum).reverse
    case (Empty(), Empty()) => accum.reverse
  }

  /** TODO Вовзращает типизированый следующий элемент */
  private def getNext: LinkedMap[K, V] = this match {
    case Cons(_, _, rest) if rest != Empty() => rest
    case Cons(_, _, rest) if rest == Empty() => Empty()
    case Empty() => Empty()
  }

  /** применяет действие `action` с побочным эффектом ко всем элементам коллекции */
  @tailrec final def foreach[U](action: ((K, V)) => U): Unit = this match {
    case Cons(k, v, rest) =>
      action(k, v)
      rest.foreach(action)
    case Empty() =>
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
