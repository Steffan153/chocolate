package chocolate

import scala.collection.{mutable => mut}
import scala.annotation.nowarn
import spire.math.Number

object Modifiers {
  val monadicModifiers: mut.Map[String, Func => Func] = mut.Map.empty
  val monadicGreedModifiers: mut.Map[String, (Func, Option[Nilad]) => Func] = mut.Map.empty
  val dyadicModifiers: mut.Map[String, (Func, Func) => Func] = mut.Map.empty
  val triadicModifiers: mut.Map[String, (Func, Func, Func) => Func] =
    mut.Map.empty
  val tetradicModifiers: mut.Map[String, (Func, Func, Func, Func) => Func] =
    mut.Map.empty

  monadicModifiers += "∇" -> {
    case a: Nilad => ???
    case f: Monad => (a: Any) => (ctx: Ctx) ?=> listIterable(a).sortWith((x, y) => lessThan(f(x), f(y))).head // Min-by.
    case a: Dyad  => (x: Any, y: Any) => (ctx: Ctx) ?=> a(y, x)(using ctx)
    case a: Triad => ???
  }
  monadicModifiers += "G" -> {
    lazy val theFunc: Func => Any => Ctx ?=> Any = f =>
      (a: Any) =>
        (ctx: Ctx) ?=>
          (a, f) match {
            case (a: CSeq, b: Nilad) =>
              LazyList.continually(b()).prependedAll(a)
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
            case (a: Any, b: Func) => theFunc(b)(Seq(a))
          }
    theFunc
  }

  private def funcs(funcs: Seq[Func], args: LazyList[Any])(using
      ctx: Ctx
  ): Any = {
    lazy val c: LazyList[Any] = args #::: c
    val i = c.iterator
    val f = funcs.reverseIterator
    var res = f.next match {
      case r: Nilad => r()
      case r: Monad => r(i.next)
      case r: Dyad  => r(i.next, i.next)
      case r: Triad => r(i.next, i.next, i.next)
    }
    while (f.hasNext) {
      res = f.next match {
        case l: Nilad => res
        case l: Monad => l(res)
        case l: Dyad  => l(res, i.next)
        case l: Triad => l(res, i.next, i.next)
      }
    }
    res
  }

  // Next two as a monad.
  dyadicModifiers += "¤" -> { (l, r) => (a: Any) => (ctx: Ctx) ?=>
    funcs(Seq(l, r), LazyList(a))
  }

  // Next three as a monad.
  triadicModifiers += "¢" -> { (l, r, o) => (a: Any) => (ctx: Ctx) ?=>
    funcs(Seq(l, r, o), LazyList(a))
  }

  // Next four as a monad.
  tetradicModifiers += "¦" -> { (l, r, o, s) => (a: Any) => (ctx: Ctx) ?=>
    funcs(Seq(l, r, o, s), LazyList(a))
  }

  // Next two as a dyad.
  dyadicModifiers += "¥" -> { (l, r) => (a: Any, b: Any) => (ctx: Ctx) ?=>
    funcs(Seq(l, r), LazyList(a, b))
  }

  // Next three as a dyad.
  triadicModifiers += "©" -> { (l, r, o) => (a: Any, b: Any) => (ctx: Ctx) ?=>
    funcs(Seq(l, r, o), LazyList(a, b))
  }

  // Next four as a dyad.
  tetradicModifiers += "§" -> {
    (l, r, o, s) => (a: Any, b: Any) => (ctx: Ctx) ?=>
      funcs(Seq(l, r, o, s), LazyList(a, b))
  }

  // Each.
  monadicModifiers += "€" -> {
    case f: Nilad => (a: Any) => (ctx: Ctx) ?=> listIterable(a).map(_ => f())
    case f: Monad => (a: Any) => (ctx: Ctx) ?=> listIterable(a).map(f(_))
    case f: Dyad =>
      (a: Any, b: Any) => (ctx: Ctx) ?=> listIterable(a).map(f(_, b))
    case f: Triad => ???
  }

  // Right-each.
  monadicModifiers += "δ" -> {
    case f: Nilad => ???
    case f: Monad => (a: Any) => (ctx: Ctx) ?=> listIterable(a).sortWith((x, y) => lessThan(f(x), f(y))).last // Max-by.
    case f: Dyad =>
      (a: Any, b: Any) => (ctx: Ctx) ?=> listIterable(b).map(f(a, _))
    case f: Triad => ???
  }

  // Dyad-to-monad.
  monadicModifiers += "ß" -> {
    case f: Nilad => ???
    case f: Monad => (a: Any) => (ctx: Ctx) ?=> listIterable(a).sortWith((x, y) => lessThan(f(x), f(y))) // Sort-by.
    case f: Dyad => (a: Any) => (ctx: Ctx) ?=> f(a, a)
    case f: Triad => (a: Any) => (ctx: Ctx) ?=> f(a, a, a)
  }

  monadicGreedModifiers += "/" -> {
    case (f: Dyad, None) =>
      (i: Any) => (ctx: Ctx) ?=>
        val a = listIterable(i)
        if (a.isEmpty) 0
        else a.reduce((x, y) => f(x, y))
    case (f: Dyad, Some(nilad)) => (i: Any) => (ctx: Ctx) ?=>
      val num = nilad().asInstanceOf[Number].toInt
      listIterable(i).grouped(num).map(a => a.reduce((x, y) => f(x, y))).toSeq
  }

  monadicGreedModifiers += "ω" -> {
    case (f: Dyad, None) =>
      (i: Any) => (ctx: Ctx) ?=>
        overlapping(listIterable(i), 2).map(a => f(a.head, a.last))
    case (f: Dyad, Some(nilad)) => (i: Any) => (ctx: Ctx) ?=>
      val num = nilad().asInstanceOf[Number]
      overlapping(listIterable(i), num).map(a => a.reduce((x, y) => f(x, y)))
  }
}
