import scala.collection.{mutable => mut}
import spire.math.Number

type Nilad = () => Any
type Monad = (Any) => Any
type Dyad = (Any, Any) => Any
type Triad = (Any, Any, Any) => Any
type Func = Nilad | Monad | Dyad | Triad

type CSeq = Seq[Any]
type CAtom = String | Number

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
  val exponent = addDyad("^")(vect2 { case (a: Number, b: Number) => a ** b })
  val negate = addMonad("N")(vect1 { case (a: Number) => -a })
  val pair = addDyad(";") { (a, b) => Seq(a, b) }
  val prefixes = addMonad("K") { case (a: String) =>
    a.scanLeft("") { (a, b) => a + b }.drop(1)
  }
  val suffixes = addMonad("#K") { case (a: String) =>
    a.scanRight("") { (a, b) => a + b }.init
  }

  class Command(val fn: Seq[Any] => Any, val arity: Int)

  object Command {
    def unapply(c: Command) = Some(c.fn, c.arity)
  }
}
