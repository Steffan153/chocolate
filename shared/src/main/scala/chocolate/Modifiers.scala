package chocolate

import scala.collection.{mutable => mut}
import scala.annotation.nowarn

@nowarn
object Modifiers {
  val monadicModifiers: mut.Map[String, Func => Func] = mut.Map.empty

  monadicModifiers += "^" -> { a => () => (ctx: Ctx) ?=> a }
  monadicModifiers += "∇" -> {
    case a: Monad => ???
    case a: Dyad => (x: Any, y: Any) => (ctx: Ctx) ?=> a(y, x)(using ctx)
    case a: Triad => ???
  }
}