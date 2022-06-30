package chocolate

import spire.math.Number
import scala.annotation.nowarn
import io.circe._
import io.circe.parser
import io.circe.generic.auto._

class CFun(val fn: Func)

class Interpreter(program: Iterator[AST]) {
  @nowarn
  def astArity(ast: AST): Int = ast match {
    case NumberLiteral(_) => 0
    case StringLiteral(_) => 0
    case Ref(_)           => 0
    case Command(s)       => Commands.getCommand(s).arity
  }
  def interpretAST(ast: AST): Any = {
    ast match {
      case NumberLiteral(n) => Number(n)
      case StringLiteral(s) => s
      case Ref(a) =>
        astArity(a) match {
          case 0 => (() => interpretAST(a))
          case 1 => (
            (l: Any) =>
              Commands.getCommand(a.asInstanceOf[Command].name).fn(Seq(l))
          )
          case 2 => (
            (l: Any, r: Any) =>
              Commands.getCommand(a.asInstanceOf[Command].name).fn(Seq(l, r))
          )
          case 3 => (
            (l: Any, r: Any, o: Any) =>
              Commands.getCommand(a.asInstanceOf[Command].name).fn(Seq(l, r, o))
          )
        }
      case Command(s) => {
        val Commands.Command(fn, arity) = Commands.getCommand(s)
        val args = 1 to arity map { _ => interpretAST(program.next) }
        fn(args)
      }
      case n => n
    }
  }
  def interpret = {
    var res = Seq[Any]()
    while (program.nonEmpty) {
      res = res :+ interpretAST(program.next)
    }
    res
  }
}

object Interpreter {
  def interpret(program: String, inputs: Seq[String]) = {
    val ctx = Ctx()
    ctx.inputs = inputs.map { x =>
      if (x.forall(x => x.isDigit || x == '.')) Number(x)
      else if (x.startsWith("[")) {
        parser.parse(x) match {
          case Right(x) =>
            lazy val f: Json => Seq[Any] = (x: Json) => x.asArray.get.map { y => y.asNumber.getOrElse { y.asString.getOrElse { f(y) } } }
            f(x)
          case Left(_) => x
        }
      }
      else x
    }
    Interpreter(
      Parser.parse(program).iterator
    ).interpret
  }
}
