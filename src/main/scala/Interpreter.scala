import spire.math.Number
import scala.annotation.nowarn

class CFun(val fn: Func)

class Interpreter(program: Iterator[AST]) {
  @nowarn
  def astArity(ast: AST): Int = ast match {
    case NumberLiteral(_) => 0
    case StringLiteral(_) => 0
    case Ref(_) => 0
    case Command(s) => Commands.getCommand(s).arity
  }
  def interpretAST(ast: AST): Any = {
    ast match {
      case NumberLiteral(n) => Number(n)
      case StringLiteral(s) => s
      case Ref(a) =>
        astArity(a) match {
          case 0 => (() => interpretAST(a))
          case 1 => ((l: Any) => Commands.getCommand(a.asInstanceOf[Command].name).fn(Seq(l)))
          case 2 => ((l: Any, r: Any) => Commands.getCommand(a.asInstanceOf[Command].name).fn(Seq(l, r)))
          case 3 => ((l: Any, r: Any, o: Any) => Commands.getCommand(a.asInstanceOf[Command].name).fn(Seq(l, r, o)))
        }
      case Command(s)       => {
        val Commands.Command(fn, arity) = Commands.getCommand(s)
        val args = 1 to arity map { _ => interpretAST(program.next) }
        fn(args)
      }
      case n => n
    }
  }
  def interpret = {
    while (program.nonEmpty) {
      chocPrint(interpretAST(program.next))
    }
  }
}

object Interpreter {
  def interpret(program: String) = Interpreter(
    Parser.parse(program).iterator
  ).interpret
}
