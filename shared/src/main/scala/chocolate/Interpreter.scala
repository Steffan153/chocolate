package chocolate

import spire.math.Number
import scala.annotation.nowarn
import io.circe._
import io.circe.parser
import io.circe.generic.auto._
import shapeless.Lazy

class CFun(val fn: Func)

class Interpreter(program: Iterator[AST]) {
  @nowarn
  def astArity(ast: AST): Int = ast match {
    case NumberLiteral(_) => 0
    case StringLiteral(_) => 0
    case Ref(_)           => 0
    case Oper(s)          => Operators.getOperator(s).arity
  }
  def interpretAST(using ctx: Ctx)(ast: AST, prog: Iterator[AST]): Any = {
    ast match {
      case NumberLiteral(n) => Number(n)
      case StringLiteral(s) => s
      case Ref(a) =>
        astArity(a) match {
          case 0 => (() => interpretAST(a, prog))
          case 1 => (
            (l: Any) => ctx ?=>
              Operators
                .getOperator(a.asInstanceOf[Oper].name)
                .fn(Seq(l))(using ctx)
          )
          case 2 => (
            (l: Any, r: Any) => ctx ?=>
              Operators
                .getOperator(a.asInstanceOf[Oper].name)
                .fn(Seq(l, r))(using ctx)
          )
          case 3 => (
            (l: Any, r: Any, o: Any) => ctx ?=>
              Operators
                .getOperator(a.asInstanceOf[Oper].name)
                .fn(Seq(l, r, o))(using ctx)
          )
        }
      case Oper(s) =>
        val Operator(fn, arity) = Operators.getOperator(s)
        val args = 1 to arity map { _ =>
          if (prog.hasNext) interpretAST(prog.next, prog)
          else interpretAST(Oper("_"), prog)
        }
        fn(args)(using ctx)
      case Lam(asts) =>
        (l: Any) => (ctx: Ctx) ?=> {
          val ct = ctx.copy
          ct.contextVar = Some(l)
          val iter = asts.iterator
          interpretAST(using ct)(iter.next, iter)
        }
      case MapLam(asts) =>
        val l = (l: Any) => (ctx: Ctx) ?=> {
          val ct = ctx.copy
          ct.contextVar = Some(l)
          val iter = asts.iterator
          interpretAST(using ct)(iter.next, iter)
        }
        val nex = if (prog.hasNext) interpretAST(prog.next, prog)
        else interpretAST(Oper("_"), prog)
        nex match {
          case x: String => x.map(i => l(i.toString))
          case x: Seq[Any] => x.map(l(_))
          case x: Number =>
            (BigInt(1) to x.toBigInt).map(x => l(Number(x)))
        }
      case n => n
    }
  }
  def interpret(using ctx: Ctx) = {
    var res = Seq[Any]()
    while (program.nonEmpty) {
      res = res :+ interpretAST(program.next, program)
    }
    res
  }
}

object Interpreter {
  def interpret(program: String, inputs: Seq[String]) = {
    given ctx: Ctx = Ctx()
    ctx.inputs = inputs.map { x =>
      if (x.forall(x => x.isDigit || x == '.')) Number(x)
      else if (x.startsWith("[") && x.endsWith("]")) {
        parser.parse(x) match {
          case Right(x) =>
            lazy val f: Json => Seq[Any] = (x: Json) =>
              x.asArray.get.map { y =>
                y.asNumber.map { x => Number(x.toBigDecimal.get) }.getOrElse { y.asString.getOrElse { f(y) } }
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
    Interpreter(
      Parser.parse(program).iterator
    ).interpret
  }
}
