package chocolate

import scala.collection.{mutable => mut}
import spire.math.Number
import spire.math.Rational.apply
import scala.annotation.nowarn

type Nilad = () => Any
type Monad = (Any) => Any
type Dyad = (Any, Any) => Any
type Triad = (Any, Any, Any) => Any
type Func = Nilad | Monad | Dyad | Triad

type CSeq = Seq[Any]
type CAtom = String | Number | Func

@nowarn
object Commands {
  val elements = mut.Map[String, Commands.Command]()

  def getCommand(name: String) = elements(name)

  private def addNilad(name: String)(fn: Nilad): Nilad = {
    elements += name -> Commands.Command((a: Seq[Any]) => fn(), 0)
    fn
  }
  private def addMonad(name: String)(fn: Monad): Monad = {
    elements += name -> Commands.Command((a: Seq[Any]) => fn(a(0)), 1)
    fn
  }
  private def addDyad(name: String)(fn: Dyad): Dyad = {
    elements += name -> Commands.Command((a: Seq[Any]) => fn(a(0), a(1)), 2)
    fn
  }
  private def addTriad(name: String)(fn: Triad): Triad = {
    elements += name -> Commands.Command(
      (a: Seq[Any]) => fn(a(0), a(1), a(2)),
      3
    )
    fn
  }

  private def vect1(fn: Monad): Monad = {
    lazy val res: Monad = {
      case x: CSeq => x.map(res)
      case x       => fn(x)
    }
    res
  }

  private def vect2(fn: Dyad): Dyad = {
    lazy val res: Dyad = {
      case (a: CSeq, b: CSeq)  => a.zip(b).map(res(_, _))
      case (a: CSeq, b: CAtom) => a.map(res(_, b))
      case (a: CAtom, b: CSeq) => b.map(res(a, _))
      case (a, b)              => fn(a, b)
    }
    res
  }

  private def vect3(fn: Triad): Triad = {
    lazy val res: Triad = {
      case (a: CSeq, b: CSeq, c: CSeq) =>
        Seq(a, b, c).transpose.map { a => res(a(0), a(1), a(2)) }
      case (a: CSeq, b: CAtom, c: CSeq)  => a.zip(c).map(res(_, b, _))
      case (a: CSeq, b: CSeq, c: CAtom)  => a.zip(b).map(res(_, _, c))
      case (a: CAtom, b: CSeq, c: CSeq)  => b.zip(c).map(res(a, _, _))
      case (a: CAtom, b: CSeq, c: CAtom) => b.map(res(a, _, c))
      case (a: CAtom, b: CAtom, c: CSeq) => c.map(res(a, b, _))
      case (a: CSeq, b: CAtom, c: CAtom) => a.map(res(_, b, c))
      case (a, b, c)                     => fn(a, b, c)
    }
    res
  }

  val add = addDyad("+")(vect2 {
    case (a: Number, b: Number) => a + b
    case (a: String, b: Number) => a + b.toString
    case (a: Number, b: String) => a.toString + b
    case (a: String, b: String) => a + b
  })
  val subtract = addDyad("-")(vect2 { case (a: Number, b: Number) => a - b })
  val multiply = addDyad("*")(vect2 {
    case (a: Number, b: Number) => a * b
    case (a: String, b: Number) => a * b.toInt
    case (a: Number, b: String) => b * a.toInt
    case (a: String, b: String) =>
      val s = suffixes(a).asInstanceOf[Seq[String]].toSet
      val p = prefixes(b).asInstanceOf[Seq[String]].toSet
      val common = s & p
      if (common.size == 0) a + b
      else
        val most = common.toSeq.maxBy(_.length)
        a.dropRight(most.length) + most + b.drop(most.length)
  })
  val divide = addDyad("/")(vect2 {
    case (a: Number, b: Number) => a / b
    case (a: String, b: String) => a.split(b).toSeq
  })
  val modulus = addDyad("%")(vect2 {
    case (a: Number, b: Number) => a tmod b
  })
  val exponent = addDyad("e")(vect2 { case (a: Number, b: Number) => a ** b })
  val negate = addMonad("N")(vect1 {
    case (a: Number) => -a
  })
  val pair = addDyad(";") { (a, b) => Seq(a, b) }
  val dropOne = addMonad("D") { case (a: Seq[Any]) => a.drop(1) }
  val concat = addDyad("C") {
    case (a: CSeq, b: CSeq) => a ++ b
    case (a: CSeq, b: CAtom) => a :+ b
    case (a: CAtom, b: CSeq) => a +: b
    case (a: Number, b: Number) => factorial(a) / (factorial(b) * factorial(a - b))
    case (a: Number, b: String) => a.toString + b
    case (a: String, b: Number) => a + b.toString
    case (a: String, b: String) => a + b
  }
  val generator = addDyad("G") {
    case (a: CSeq, b: Nilad) => LazyList.continually(b()).prependedAll(a)
    case (a: CSeq, b: Monad) => LazyList.iterate(a.last)(b).prependedAll(a.init)
    case (a: CSeq, b: Dyad) =>
      lazy val seq: LazyList[Any] = a.to(LazyList) #::: seq.zip(seq.tail).map(b(_, _))
      seq
  }
  val prefixes = addMonad("P") {
    case (a: Number) =>
      var divisors = Seq[Number]()
      var i = Number.one
      while (i <= a / 2) {
        if (a.tmod(i).isZero) divisors = divisors :+ i
        i += 1
      }
      divisors :+ a
    case (a: String) => a.scanLeft("")(_ + _).drop(1)
    case (a: LazyList[Any]) => a.scanLeft(LazyList[Any]()){_ appended _}.drop(1)
    case (a: CSeq) => a.scanLeft(Seq[Any]()){_ appended _}.drop(1)
  }
  val sliceUntil = addDyad(":") {
    case (a: LazyList[Any], b: Number) => a.take(b.toInt)
    case (a: CSeq, b: Number) => a.take(b.toInt)
    case (a: String, b: Number) => a.take(b.toInt)
  }
  val suffixes = addMonad("#s") {
    case (a: String) => a.scanRight("")(_ +: _).init
  }
  val fibonacci = addMonad("#f") {
    case (n: Number) =>
      var a = Number.zero
      var b = Number.one
      var i = Number.zero
      while (i < n) {
        val temp = a
        a = b
        b = temp + b
        i += 1
      }
      a
  }
  val factorial = addMonad("!") {
    case (a: Number) =>
      var p = Number.one
      var i = Number.one
      while (i <= a) {
        p *= i
        i += 1
      }
      p
  }.asInstanceOf[Any => Number]
  val infiniteNumbers = addNilad("#i") { () =>
    lazy val l: LazyList[Number] = Number.one #:: l.map(_ + 1)
    l
  }
  addNilad("c1") { () => Seq(Number.one, Number.one) }

  class Command(val fn: Seq[Any] => Any, val arity: Int)

  object Command {
    def unapply(c: Command) = Some(c.fn, c.arity)
  }
}
