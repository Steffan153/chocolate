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
    case Command(s)       => Commands.getCommand(s).arity
  }
  def interpretAST(using ctx: Ctx)(ast: AST): Any = {
    ast match {
      case NumberLiteral(n) => Number(n)
      case StringLiteral(s) => s
      case Ref(a) =>
        astArity(a) match {
          case 0 => (() => interpretAST(a))
          case 1 => (
            (l: Any) => ctx ?=>
              Commands.getCommand(a.asInstanceOf[Command].name).fn(Seq(l))(using ctx)
          )
          case 2 => (
            (l: Any, r: Any) => ctx ?=>
              Commands.getCommand(a.asInstanceOf[Command].name).fn(Seq(l, r))(using ctx)
          )
          case 3 => (
            (l: Any, r: Any, o: Any) => ctx ?=>
              Commands.getCommand(a.asInstanceOf[Command].name).fn(Seq(l, r, o))(using ctx)
          )
        }
      case Command(s) => {
        val Commands.Command(fn, arity) = Commands.getCommand(s)
        val args = 1 to arity map { _ =>
          if (program.hasNext) interpretAST(program.next)
          else interpretAST(Command("?"))
        }
        fn(args)(using ctx)
      }
      case n => n
    }
  }
  def interpret(using ctx: Ctx) = {
    var res = Seq[Any]()
    while (program.nonEmpty) {
      res = res :+ interpretAST(program.next)
    }
    res
  }
}

object Interpreter {
  def interpret(program: String, inputs: Seq[String]) = {
    given ctx: Ctx = Ctx()
    ctx.inputs = inputs.map { x =>
      if (x.forall(x => x.isDigit || x == '.')) Number(x)
      else if (x.startsWith("[")) {
        parser.parse(x) match {
          case Right(x) =>
            lazy val f: Json => Seq[Any] = (x: Json) =>
              x.asArray.get.map { y =>
                y.asNumber.getOrElse { y.asString.getOrElse { f(y) } }
              }
            f(x)
          case Left(_) => x
        }
      } else x
    }
    lazy val temp: LazyList[Any] = ctx.inputs.to(LazyList) #::: temp
    ctx.inputCycle = temp.iterator
    Interpreter(
      Parser.parse(program).iterator
    ).interpret
  }
}
