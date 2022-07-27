package chocolate

import scala.collection.mutable
import scala.annotation.nowarn
import spire.math.Number
import spire.math.Rational.apply

def chocPrint(s: Any) = {
  @nowarn
  lazy val res: (Any, Boolean, Boolean) => Any = {
    case (s: String, quotify: Boolean, _) =>
      if (quotify)
        s"\"${s.replace("\\", "\\\\").replace("\n", "\\n").replace("\t", "\\t").replace("\"", "\\\"")}\""
      else s
    case (s: spire.math.Number, _, _) => s
    case (s: LazyList[Any], _, forceEval: Boolean) =>
      if (forceEval)
        "[" + s.mkString(", ") + "]"
      else
        () => {
          print("[")
          s.dropRight(1).foreach { x =>
            print(res(x, true, true))
            print(", ")
          }
          print(res(s.last, true, true))
          println("]")
        }
    case (s: Seq[Any], _, _) =>
      "[" + s.map(res(_, true, true)).mkString(", ") + "]"
  }
  res(s, true, false) match {
    case (x: (() => Any)) => x()
    case x                => println(x)
  }
}

def chop(a: String, b: Number) = {
  val chunkLen = (a.length / b).toInt
  val leftOver = a.length.tmod(b)
  var f = 0
  (0 until b.toInt).map { i =>
    val len = chunkLen + (if (leftOver > i) 1 else 0)
    f += len
    a.drop(f - len).take(len)
  }
}

def boolify(a: Any): Boolean = {
  a match {
    case a: CSeq   => a.nonEmpty
    case a: String => a.nonEmpty
    case a: Number => !a.isZero
  }
}

def spireRange(a: Number, b: Number): Seq[Number] = {
  LazyList.unfold(a) { s => if (s > b) None else Some((s, s + 1)) }
}

def boolToNum(a: Boolean) = if (a) Number.one else Number.zero

def iterable(a: Any): Seq[Any] | String = {
  a match {
    case a: Number => spireRange(1, a)
    case a: String => a
    case a: Seq[Any] => a
  }
}

def listIterable(a: Any): Seq[Any] = {
  a match {
    case a: Number => spireRange(1, a)
    case a: String => a.map(_.toString)
    case a: Seq[Any] => a
  }
}

def astToFunc(at: AST): Func = {
  at match {
    case _: (NumberLiteral | StringLiteral | SeqBuild | Const) =>
      () => (ctx: Ctx) ?=> Interpreter.interpretAST(using ctx)(at, Iterator.empty)
    case MonadicModified(ast, mod) => Modifiers.monadicModifiers(mod)(astToFunc(ast))
    case DyadicModified(ast1, ast2, mod) =>
      Modifiers.dyadicModifiers(mod)(astToFunc(ast1), astToFunc(ast2))
    case MapLam(_) =>
      (x: Any) => (ctx: Ctx) ?=> Interpreter.interpretAST(using ctx)(at, Iterator(Const(x)))
    case WhiteSpace() =>
      throw InternalError(
        "When modifying an AST, a white space character was received - this is probably a bug."
      )
    case CloseChar() =>
      throw Exception(
        "Huh? Modifying a closing character? What's that supposed to do?"
      )
    case o: Oper =>
      val Operator(fn, arity) = Operators.getOperator(o.name)
      arity match {
        case 0 => () => (ctx: Ctx) ?=> fn(Seq())(using ctx)
        case 1 => (x: Any) => (ctx: Ctx) ?=> fn(Seq(x))(using ctx)
        case 2 => (x: Any, y: Any) => (ctx: Ctx) ?=> fn(Seq(x, y))(using ctx)
        case 3 =>
          (x: Any, y: Any, z: Any) => (ctx: Ctx) ?=> fn(Seq(x, y, z))(using ctx)
      }
  }
}
