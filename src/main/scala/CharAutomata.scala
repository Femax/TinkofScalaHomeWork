import CharAutomata._

trait CharAutomata[+A] {

  /** потребить один символ и перейти в следующее состояние */
  def consume(char: Char): CharAutomata[A]

  /** получить текущий результат, если это конец строки */
  def result: Either[String, A]

  /** потребить строку символ за символом */
  def apply(source: String): Either[String, A] = source match {
    case `source` if source.length > 0 =>
//      println(s"source is $source")
      val cons = consume(source.charAt(0))
//      println(s"cons is ${cons.result}")
      cons match {
        case a: Error => a.result
        case _ => cons.apply(source.substring(1))
      }
    case _ => result
  }


  /** создать автомат, который запустит оба автомата
    * и если первый вернёт ошибку, вернёт результат второго
    */
  def or[B](auto: CharAutomata[B]): CharAutomata[Either[A, B]] = new Or[A, B](this, auto)

  /** создать автомат, который запустит оба автомата
    * вернёт результаты обоих, если оба вернут успешный результат
    * и вернёт ошибку, если вернёт ошибку хотя бы один
    */
  def and[B](auto: CharAutomata[B]): CharAutomata[(A, B)] = new And[A, B](this, auto)
}

object CharAutomata {

  /** создаёт автомат, всегда результирующий с ошибкой
    * с заданным текстом message
    */
  def error(message: String): CharAutomata[Nothing] = new Error(message)

  /** создаёт автомат, всегда успешно результирующий
    * с заданным значением `value`
    */
  def const[A](value: A): CharAutomata[A] = new Const[A](value)

  /** создаёт автомат, возвращающий
    * первое вхождение строчки `substring`
    * или ошибкой
    */
  def find(substring: String): CharAutomata[Int] =
    if (substring.isEmpty)
      new Const(0)
    else {
      println(substring)
      new Find(substring)()
    }


  /** создаёт автомат, определяющий является ли строчка,
    * если исключить из неё все символы, кроме `'('` и `')'`
    * корректной скобочной последовательностью */
  def balance: CharAutomata[Unit] = new ParenBalance(0)

  /** создаёт автомат, ищущий первое число, из цифр подряд
    * и возвращающий результат в качестве BigInt либо 0
    */
  def parseInt: CharAutomata[BigInt] = new ParseInteger()

  /** класс для реализации метода `error` */
  class Error(string: String) extends CharAutomata[Nothing] {
    def consume(char: Char): CharAutomata[Nothing] = this


    def result: Either[String, Nothing] = Left(string)
  }

  /** класс для реализации метода `const` */
  class Const[A] private[CharAutomata](value: A) extends CharAutomata[A] {
    def consume(char: Char): CharAutomata[A] = this

    def result: Either[String, A] = Right(value)
  }

  /** класс для реализации метода `find` */
  class Find private[CharAutomata](substring: String)(val charAutomataPosition: Int = 0, val subStrPosition: Int = 0) extends CharAutomata[Int] {

    def consume(char: Char): CharAutomata[Int] = {
      println(charAutomataPosition)
      println(substring.indexOf(char))
      if (result.isLeft) {
        char match {
          case ch if substring.indexOf(ch, subStrPosition) == subStrPosition => new Find(substring)(charAutomataPosition + 1, subStrPosition + 1)
          case ch if substring.indexOf(ch) != subStrPosition => new Find(substring)(charAutomataPosition + 1, 0)
        }
      }
      else this
    }

    def result: Either[String, Int] = subStrPosition match {
      case pos if pos == substring.length => Right(charAutomataPosition - subStrPosition)
      case pos if pos != substring.length => Left("Not found")
    }
  }

  /** класс для реализации метода `balance` */
  class ParenBalance private[CharAutomata](val count: Int) extends CharAutomata[Unit] {
    def consume(char: Char): CharAutomata[Unit] = char match {
      case '(' => new ParenBalance(count + 1)
      case ')' if count > 0 => new ParenBalance(count - 1)
      case ')' if count == 0 => new Error("Not valid")
      case _ => new ParenBalance(count)
    }


    def result: Either[String, Unit] =
      if (count == 0)
        Right()
      else
        Left("Not valid")
  }


  /** класс для реализации метода `parseInt` */
  class ParseInteger private[CharAutomata](strInt: StringBuilder = new StringBuilder(), end: Boolean = false) extends CharAutomata[BigInt] {
    def consume(char: Char): CharAutomata[BigInt] = if (!end) {
      char match {
        case `char` if "[1-9]".r.findFirstIn(char.toString).isDefined => new ParseInteger(strInt + char)
        case _ if strInt.nonEmpty => new ParseInteger(strInt, true)
        case _ if strInt.isEmpty => new ParseInteger(strInt, end)
      }
    } else {
      this
    }

    def result: Either[String, BigInt] = if (end) {
      Right(strInt.toString().toInt)
    } else {
      Right(0)
    }
  }

  /** класс для реализации метода `and` */
  class And[A, B] private[CharAutomata](autoA: CharAutomata[A], autoB: CharAutomata[B]) extends CharAutomata[(A, B)] {
    def consume(char: Char): CharAutomata[(A, B)] = new And[A, B](autoA.consume(char), autoB.consume(char))

    def result: Either[String, (A, B)] = (autoA.result, autoB.result) match {
      case (Left(a: String), Left(b: String)) => Left(a)
      case (Left(a: String), Right(b: B)) => Left(a)
      case (Right(a: A), Left(b: String)) => Left(b)
      case (Right(a: A), Right(b: B)) => Right((a, b))
    }
  }

  /** класс для реализации метода `or` */
  class Or[A, B] private[CharAutomata](autoA: CharAutomata[A], autoB: CharAutomata[B]) extends CharAutomata[Either[A, B]] {
    def consume(char: Char): CharAutomata[Either[A, B]] = new Or[A, B](autoA.consume(char), autoB.consume(char))

    def result: Either[String, Either[A, B]] = (autoA.result, autoB.result) match {
      case (Left(a: String), Left(_: String)) => Left(a)
      case (Left(_: String), Right(b: B)) => Right(Right(b))
      case (Right(a: A), Left(_: String)) => Right(Left(a))
      case (Right(a: A), Right(_: B)) => Right(Left(a))
    }

  }

}