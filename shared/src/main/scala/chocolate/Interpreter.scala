package chocolate

import spire.math.Number
import scala.annotation.nowarn
import io.circe._
import io.circe.parser
import io.circe.generic.auto._
import shapeless.Lazy

class CFun(val fn: Func)

class Interpreter(program: Iterator[AST]) {
  def interpretAST(using ctx: Ctx)(ast: AST, prog: Iterator[AST]): Any = {
    ast match {
      case NumberLiteral(n) => Number(n)
      case StringLiteral(s) => s
      case Oper(s) =>
        val Operator(fn, arity) = Operators.getOperator(s)
        val args = 1 to arity map { _ =>
          if (prog.hasNext) interpretAST(prog.next, prog)
          else interpretAST(Oper("_"), prog)
        }
        fn(args)(using ctx)
      case MonadicModified(at, mod) =>
        lazy val func: AST => Func = { at => at match {
          case _: NumberLiteral => () => (ctx: Ctx) ?=> interpretAST(using ctx)(at, prog)
          case _: StringLiteral => () => (ctx: Ctx) ?=> interpretAST(using ctx)(at, prog)
          case _: Lam => () => (ctx: Ctx) ?=> interpretAST(using ctx)(at, prog)
          case MonadicModified(ast, mod) => Modifiers.monadicModifiers(mod)(func(ast))
          case o: Oper =>
            val Operator(fn, arity) = Operators.getOperator(o.name)
            arity match {
              case 0 => () => (ctx: Ctx) ?=> fn(Seq())(using ctx)
              case 1 => (x: Any) => (ctx: Ctx) ?=> fn(Seq(x))(using ctx)
              case 2 => (x: Any, y: Any) => (ctx: Ctx) ?=> fn(Seq(x, y))(using ctx)
              case 3 => (x: Any, y: Any, z: Any) => (ctx: Ctx) ?=> fn(Seq(x, y, z))(using ctx)
            }
        } }
        val getArg = { () =>
          if (prog.hasNext) interpretAST(prog.next, prog)
          else interpretAST(Oper("_"), prog)
        }
        Modifiers.monadicModifiers(mod)(func(at)) match {
          case f: Nilad => f()(using ctx)
          case f: Monad => f(getArg())(using ctx)
          case f: Dyad => f(getArg(), getArg())(using ctx)
          case f: Triad => f(getArg(), getArg(), getArg())(using ctx)
        }
      case Lam(asts) =>
        var arity = 1
        var ats = asts
        if (asts.length > 1 && asts.head.isInstanceOf[NumberLiteral]) {
          arity = asts.head.asInstanceOf[NumberLiteral].value.toInt
          ats = asts.tail
        }
        arity match {
          case 0 => () => (ctx: Ctx) ?=> {
            val iter = ats.iterator
            interpretAST(using ctx)(iter.next, iter)
          }
          case 1 => (l: Any) => (ctx: Ctx) ?=> {
            val ct = ctx.copy
            ct.contextVars = Iterator.continually(l)
            val iter = ats.iterator
            interpretAST(using ct)(iter.next, iter)
          }
          case 2 => (l: Any, r: Any) => (ctx: Ctx) ?=> {
            val ct = ctx.copy
            lazy val temp: LazyList[Any] = l #:: r #:: temp
            ct.contextVars = temp.iterator
            ct.contextVarsSeq = Seq(l, r)
            val iter = ats.iterator
            interpretAST(using ct)(iter.next, iter)
          }
          case 3 => (l: Any, r: Any, o: Any) => (ctx: Ctx) ?=> {
            val ct = ctx.copy
            lazy val temp: LazyList[Any] = l #:: r #:: o #:: temp
            ct.contextVars = temp.iterator
            ct.contextVarsSeq = Seq(l, r, o)
            val iter = ats.iterator
            interpretAST(using ct)(iter.next, iter)
          }
          case n => throw new Exception("Invalid arity of function, should be 1, 2, or 3")
        }
      case MapLam(asts) =>
        val l = (l: Any) => (ctx: Ctx) ?=> {
          val ct = ctx.copy
          ct.contextVars = Iterator.continually(l)
          ct.contextVarsSeq = Seq(l)
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
      case SeqBuild(asts) =>
        val iter = asts.iterator
        var seq = Seq[Any]()
        while (iter.hasNext) {
          seq = seq :+ interpretAST(iter.next, iter)
        }
        seq
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
