package chocolate

import scala.collection.{mutable => mut}
import scala.annotation.nowarn
import spire.math.Number

object Modifiers {
  val monadicModifiers: mut.Map[String, Func => Func] = mut.Map.empty
  val dyadicModifiers: mut.Map[String, (Func, Func) => Func] = mut.Map.empty
  val triadicModifiers: mut.Map[String, (Func, Func, Func) => Func] = mut.Map.empty
  val tetradicModifiers: mut.Map[String, (Func, Func, Func, Func) => Func] = mut.Map.empty

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

  private def twoFuncs(l: Func, r: Func, args: LazyList[Any])(using ctx: Ctx): Any = {
    lazy val c: LazyList[Any] = args #::: c
    val i = c.iterator
    val right = r match {
      case r: Nilad => r()
      case r: Monad => r(i.next)
      case r: Dyad => r(i.next, i.next)
      case r: Triad => r(i.next, i.next, i.next)
    }
    l match {
      case l: Nilad => right
      case l: Monad => l(right)
      case l: Dyad => l(right, i.next)
      case l: Triad => l(right, i.next, i.next)
    }
  }

  // Next two as a monad.
  dyadicModifiers += "¤" -> { (l, r) => (a: Any) => (ctx: Ctx) ?=> twoFuncs(l, r, LazyList(a)) }

  // Next two as a dyad.
  dyadicModifiers += "¥" -> { (l, r) => (a: Any, b: Any) => (ctx: Ctx) ?=> twoFuncs(l, r, LazyList(a, b)) }

  // Each.
  monadicModifiers += "€" -> {
    case f: Nilad => (a: Any) => (ctx: Ctx) ?=> listIterable(a).map(_ => f())
    case f: Monad => (a: Any) => (ctx: Ctx) ?=> listIterable(a).map(f(_))
    case f: Dyad => (a: Any, b: Any) => (ctx: Ctx) ?=> (a, b) match {
      case (a: CSeq, b: CSeq) => a.map(f(_, b))
      case (a: CSeq, b: CAtom) => a.map(f(_, b))
      case (a: CAtom, b: CSeq) => b.map(f(a, _))
      case (a: String, b: String) => a.map(f(_, b))
      case (a: String, b: Number) => a.map(f(_, b))
      case (a: Number, b: String) => b.map(f(a, _))
      case (a: Number, b: Number) => listIterable(a).map(f(_, b))
    }
    case f: Triad => ???
  }
}
