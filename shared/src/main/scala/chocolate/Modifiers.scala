package chocolate

import scala.collection.{mutable => mut}
import scala.annotation.nowarn
import spire.math.Number

@nowarn
object Modifiers {
  val monadicModifiers: mut.Map[String, Func => Func] = mut.Map.empty
  val dyadicModifiers: mut.Map[String, (Func, Func) => Func] = mut.Map.empty

  monadicModifiers += "∇" -> {
    case a: Nilad => ???
    case a: Monad =>
      (x: Any) =>
        (ctx: Ctx) ?=>
          x match {
            case x: CSeq => x.filter { c => boolify(a(c)(using ctx)) }
            case x: String =>
              x.filter { c => boolify(a(c.toString)(using ctx)) }
            case x: Number =>
              spireRange(Number.one, x).filter { c => boolify(a(c)(using ctx)) }
          }
    case a: Dyad  => (x: Any, y: Any) => (ctx: Ctx) ?=> a(y, x)(using ctx)
    case a: Triad => ???
  }
  monadicModifiers += "G" -> { f => (a: Any) => (ctx: Ctx) ?=>
    (a, f) match {
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
  }

  dyadicModifiers += "¤" -> { (l, r) => (a: Any) => (ctx: Ctx) ?=>
    (l, r) match {
      case (l: Nilad, r: Nilad) => r()
      case (l: Nilad, r: Monad) => r(a)
      case (l: Nilad, r: Dyad) => r(a, a)
      case (l: Nilad, r: Triad) => r(a, a, a)
      case (l: Monad, r: Nilad) => l(r())
      case (l: Monad, r: Monad) => l(r(a))
      case (l: Monad, r: Dyad) => l(r(a, a))
      case (l: Monad, r: Triad) => l(r(a, a, a))
      case (l: Dyad, r: Nilad) => l(r(), a)
      case (l: Dyad, r: Monad) => l(r(a), a)
      case (l: Dyad, r: Dyad) => l(r(a, a), a)
      case (l: Dyad, r: Triad) => l(r(a, a, a), a)
      case (l: Triad, r: Nilad) => l(r(), a, a)
      case (l: Triad, r: Monad) => l(r(a), a, a)
      case (l: Triad, r: Dyad) => l(r(a, a), a, a)
      case (l: Triad, r: Triad) => l(r(a, a, a), a, a)
    }
  }
}
