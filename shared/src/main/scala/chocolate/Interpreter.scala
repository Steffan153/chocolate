package chocolate

import spire.math.Number
import scala.annotation.nowarn
import io.circe._
import io.circe.parser
import io.circe.generic.auto._
import shapeless.Lazy

class CFun(val fn: Func)

object Interpreter {
  def interpretIter(program: Iterator[AST])(using ctx: Ctx) = {
    var res = Seq[Any]()
    while (program.nonEmpty) {
      res = res :+ Interpreter.interpretAST(program.next, program)
    }
    res
  }

  private def callFunc(f: Func, prog: Iterator[AST])(using ctx: Ctx) = {
    val getArg = { () =>
      if (prog.hasNext) interpretAST(prog.next, prog)
      else interpretAST(Oper("_"), prog)
    }
    f match {
      case f: Nilad => f()(using ctx)
      case f: Monad => f(getArg())(using ctx)
      case f: Dyad  => f(getArg(), getArg())(using ctx)
      case f: Triad => f(getArg(), getArg(), getArg())(using ctx)
    }
  }

  def interpretAST(using ctx: Ctx)(ast: AST, prog: Iterator[AST]): Any = {
    ast match {
      case Const(x)         => x
      case NumberLiteral(n) => Number(n)
      case StringLiteral(s) => s
      case Oper(s) =>
        val Operator(fn, arity) = Operators.getOperator(s)
        val args = 1 to arity map { _ =>
          if (prog.hasNext) interpretAST(prog.next, prog)
          else interpretAST(Oper("_"), prog)
        }
        fn(args)(using ctx)
      case MonadicGreedModified(at, nil, mod) =>
        callFunc(Modifiers.monadicGreedModifiers(mod)(astToFunc(at), nil.map(astToFunc(_).asInstanceOf[Nilad])), prog)
      case MonadicModified(at, mod) =>
        callFunc(Modifiers.monadicModifiers(mod)(astToFunc(at)), prog)
      case DyadicModified(at, at2, mod) =>
        callFunc(Modifiers.dyadicModifiers(mod)(astToFunc(at), astToFunc(at2)), prog)
      case TriadicModified(at, at2, at3, mod) =>
        callFunc(Modifiers.triadicModifiers(mod)(astToFunc(at), astToFunc(at2), astToFunc(at3)), prog)
      case TetradicModified(at, at2, at3, at4, mod) =>
        callFunc(Modifiers.tetradicModifiers(mod)(astToFunc(at), astToFunc(at2), astToFunc(at3), astToFunc(at4)), prog)
      case MapLam(asts) =>
        val l = (l: Any) =>
          (ctx: Ctx) ?=> {
            val ct = ctx.copy
            ct.contextVars = Iterator.continually(l)
            ct.contextVarsSeq = Seq(l)
            if (asts.isEmpty) {
              return l
            }
            val iter = asts.iterator
            interpretAST(using ct)(iter.next, iter)
          }
        val nex =
          if (prog.hasNext) interpretAST(prog.next, prog)
          else interpretAST(Oper("_"), prog)
        nex match {
          case x: String   => x.map(i => l(i.toString))
          case x: Seq[Any] => x.map(l(_))
          case x: Number =>
            spireRange(1, x).map(l(_))
        }
      case CloseChar() =>
        throw Exception("You can't close something that hasn't been opened.")
      case SeqBuild(asts) =>
        val iter = asts.iterator
        var seq = Seq[Any]()
        while (iter.hasNext) {
          seq = seq :+ interpretAST(iter.next, iter)
        }
        seq
      case WhiteSpace() =>
        throw InternalError("White space received in interpreter that should have been filtered out by the parser. This is a bug.")
    }
  }

  def interpret(program: String, inputs: Seq[String]) = {
    given ctx: Ctx = Ctx()
    ctx.inputs = inputs.map { x =>
      if (x.forall(x => x.isDigit || x == '.')) Number(x)
      else if (x.startsWith("[") && x.endsWith("]")) {
        parser.parse(x) match {
          case Right(x) =>
            lazy val f: Json => Seq[Any] = (x: Json) =>
              x.asArray.get.map { y =>
                y.asNumber.map { x => Number(x.toBigDecimal.get) }.getOrElse {
                  y.asString.getOrElse { f(y) }
                }
              }
            f(x)
          case Left(_) => x
        }
      } else if (x.startsWith("\"") && x.endsWith("\""))
        x.init.tail
          .replace("\\\"", "\"")
          .replace("\\n", "\n")
          .replace("\\\\", "\\")
      else x
    }
    lazy val temp: LazyList[Any] = ctx.inputs.to(LazyList) #::: temp
    ctx.inputCycle = temp.iterator
    interpretIter(
      Parser.parse(program).iterator
    )
  }
}
