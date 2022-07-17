package chocolate

import scala.collection.{mutable => mut}
import spire.math.Number
import spire.math.Rational.apply
import scala.annotation.nowarn

type Nilad = () => Ctx ?=> Any
type Monad = Any => Ctx ?=> Any
type Dyad = (Any, Any) => Ctx ?=> Any
type Triad = (Any, Any, Any) => Ctx ?=> Any
type Func = Nilad | Monad | Dyad | Triad

type CSeq = Seq[Any]
type CAtom = String | Number | Func

@nowarn
object Operators {
  val elements = mut.Map[String, Operator]()

  def getOperator(name: String) = elements(name)

  private def addNilad(name: String)(fn: Nilad): Nilad = {
    elements += name -> Operator((a: Seq[Any]) => fn(), 0)
    fn
  }
  private def addMonad(name: String)(fn: Monad): Monad = {
    elements += name -> Operator((a: Seq[Any]) => fn(a(0)), 1)
    fn
  }
  private def addDyad(name: String)(fn: Dyad): Dyad = {
    elements += name -> Operator((a: Seq[Any]) => fn(a(0), a(1)), 2)
    fn
  }
  private def addTriad(name: String)(fn: Triad): Triad = {
    elements += name -> Operator((a: Seq[Any]) => fn(a(0), a(1), a(2)), 3)
    fn
  }

  private def vect1(fn: Monad): Monad = {
    lazy val res: Monad = {
      case x: CSeq => x.map(res(_))
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
    case (a: String, b: Number) => chop(a, b)
    case (a: Number, b: String) => chop(b, a)
    case (a: String, b: String) => a.split(b).toSeq
  })
  val modulus = addDyad("%")(vect2 {
    case (a: Number, b: Number) => a tmod b
    case (a: String, b: Number) => ???
    case (a: Number, b: String) => ???
    case (a: String, b: String) => ???
  })
  val exponent: Dyad = addDyad("e")(vect2 { case (a: Number, b: Number) =>
    a ** b
  })
  val negate = addMonad("N")(vect1 {
    case (a: Number) => -a
    case (a: String) => a.map { x => if (x.isUpper) x.toLower else x.toUpper }
  })
  val pair = addDyad(";") { (a, b) => Seq(a, b) }
  val dropOne = addMonad("D") {
    case (a: Seq[Any]) => a.drop(1)
    case (a: String)   => a.drop(1)
    case (a: Number)   => ???
  }
  val reverse = addMonad("⅃") {
    case (a: Seq[Any]) => a.reverse
    case (a: String)   => a.reverse
    case (a: Number)   => ???
  }
  val binomial = addDyad("B")(vect2 {
    case (a: Number, b: Number) =>
      factorial(a).asInstanceOf[Number] / (factorial(b)
        .asInstanceOf[Number] * factorial(a - b).asInstanceOf[Number])
    case (a: String, b: Number) => ???
    case (a: Number, b: String) => ???
    case (a: String, b: String) => ???
  })
  val concat = addDyad("C") {
    case (a: CSeq, b: CSeq)     => a ++ b
    case (a: CSeq, b: CAtom)    => a :+ b
    case (a: CAtom, b: CSeq)    => a +: b
    case (a: Number, b: Number) => ???
    case (a: Number, b: String) => a.toString + b
    case (a: String, b: Number) => a + b.toString
    case (a: String, b: String) => a + b
  }
  val head = addMonad("Ḣ") {
    case (a: Number) => ???
    case (a: String) => a.take(1)
    case (a: CSeq) => a.head
  }
  val double = addMonad("Д") {
    case (a: Number) => a * 2
  }
  val generator: Dyad = addDyad("G") {
    case (a: Func, b: CSeq)              => generator(b, a)
    case (a: Func, b: (Number | String)) => generator(Seq(b), a)
    case (a: (Number | String), b: Func) => generator(Seq(a), b)
    case (a: CSeq, b: Nilad) => LazyList.continually(b()).prependedAll(a)
    case (a: CSeq, b: Monad) =>
      LazyList.iterate(a.last)(b(_)).prependedAll(a.init)
    case (a: CSeq, b: Dyad) =>
      LazyList
        .unfold((a.init.last, a.last)) { s =>
          val v = b(s._1, s._2)
          Some((v, (s._2, v)))
        }
        .prependedAll(a)
    case (a: CSeq, b: Triad) =>
      LazyList
        .unfold((a.dropRight(2).last, a.init.last, a.last)) { s =>
          val v = b(s._1, s._2, s._3)
          Some((v, (s._2, s._3, v)))
        }
        .prependedAll(a)
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
    case (a: LazyList[Any]) =>
      a.scanLeft(LazyList[Any]()) { _ appended _ }.drop(1)
    case (a: CSeq) => a.scanLeft(Seq[Any]()) { _ appended _ }.drop(1)
  }
  val ord = addMonad("O")(vect1 {
    case (a: Number) => a.toChar.toString
    case (a: String) => if (a.length == 1) Number(a.head) else a.map(Number(_))
  })
  val flat = addMonad("F") {
    case (a: Seq[CSeq]) => a.flatten
  }
  val transliterate = addTriad("∂") {
    case (b: String, c: String, a: String) => a.map { x => if (b contains x) c(b.indexOf(x)) else x }
  }
  val sliceUntil: Dyad = addDyad(":") {
    case (a: CSeq, b: Number)          => a.take(b.toInt)
    case (a: String, b: Number)        => a.take(b.toInt)
    case (a: Number, b: CSeq)          => b.take(a.toInt)
    case (a: Number, b: String)        => b.take(a.toInt)
    case (a: Monad, b: CSeq)           => b.map(a(_))
    case (a: Monad, b: String)         => b.map { x => a(x.toString) }
    case (a: Any, b: Monad)            => sliceUntil(b, a)
  }
  val suffixes = addMonad("#s") { case (a: String) =>
    a.scanRight("")(_ +: _).init
  }
  val fibonacci = addMonad("#f") { case (n: Number) =>
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
  val factorial = addMonad("!") { case (a: Number) =>
    var p = Number.one
    var i = Number.one
    while (i <= a) {
      p *= i
      i += 1
    }
    p
  }
  val interleave = addDyad("Ỵ") {
    case (a: String, b: String) =>
      val ai = a.iterator
      val bi = b.iterator
      val str = new StringBuilder
      while (ai.hasNext || bi.hasNext) {
        if (ai.hasNext) str.append(ai.next)
        if (bi.hasNext) str.append(bi.next)
      }
      str.mkString
    case (a: CSeq, b: CSeq) =>
      val ai = a.iterator
      val bi = b.iterator
      LazyList.unfold((ai, bi)) {
        (a, b) =>
          if (a.hasNext && !b.hasNext) Some((Seq(a.next), (a, b)))
          else if (!a.hasNext && b.hasNext) Some((Seq(b.next), (a, b)))
          else if (a.hasNext && b.hasNext) Some((Seq(a.next, b.next), (a, b)))
          else None
      }.flatten
  }

  addNilad("#i") { () =>
    lazy val a: LazyList[Number] = Number.one #:: a.map(_ + 1)
    a
  }
  addNilad("#I") { () =>
    lazy val a: LazyList[Number] = Number.one #:: -Number.one #:: a.map { x => if (x < 0) x - 1 else x + 1 }
    Number.zero #:: a
  }
  addNilad("#Ḟ") { () =>
    Number.zero #:: Number.one #:: LazyList.unfold((Number.zero, Number.one)) { s =>
      val v = s._1 + s._2
      Some((v, (s._2, v)))
    }
  }
  addNilad("?") { () => (ctx: Ctx) ?=> ctx.inputCycle.next() }
  addNilad("_") { () => (ctx: Ctx) ?=> if (ctx.contextVars.hasNext) ctx.contextVars.next() else ctx.inputCycle.next() }
  addNilad("#_") { () => (ctx: Ctx) ?=> ctx.contextVarsSeq }
  addNilad("#0") { () => (ctx: Ctx) ?=> ctx.contextVarsSeq.head }
  addNilad("#1") { () => (ctx: Ctx) ?=> ctx.contextVarsSeq(1) }
  addNilad("c1") { () => Seq(Number.one, Number.one) }
  addNilad("ca") { () => "abcdefghijklmnopqrstuvwxyz" }
  addNilad("cA") { () => "ABCDEFGHIJKLMNOPQRSTUVWXYZ" }
  addNilad("cZ") { () => "ZYXWVUTSRQPONMLKJIHGFEDCBA" }
  addNilad("cz") { () => "zyxwvutsrqponmlkjihgfedcba" }
  addNilad("cb") { () => "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" }
  addNilad("cy") { () => "zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA" }
  addNilad("cc") { () => "bcdfghjklmnpqrstvwxyz" }
  addNilad("cċ") { () => "bcdfghjklmnpqrstvwxz" }
  addNilad("cv") { () => "aeiou" }
  addNilad("cṽ") { () => "aeiouy" }
  addNilad("cV") { () => "AEIOU" }
  addNilad("cṿ") { () => "aeiouAEIOU" }
}

class Operator(val fn: Seq[Any] => Ctx ?=> Any, val arity: Int)

object Operator {
  def unapply(c: Operator) = Some(c.fn, c.arity)
}
