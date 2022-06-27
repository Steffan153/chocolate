import spire.math.Number

class Interpreter(program: Iterator[AST]) {
  def interpretAST(): Any = {
    program.next match {
      case NumberLiteral(n) => Number(n)
      case StringLiteral(s) => s
      case Command(s)       => {
        val Commands.Command(fn, arity) = Commands.getCommand(s)
        val args = 1 to arity map { _ => interpretAST() }
        fn(args)
      }
      case n => n
    }
  }
  def interpret = {
    while (program.nonEmpty) {
      chocPrint(interpretAST())
    }
  }
}

object Interpreter {
  def interpret(program: String) = Interpreter(
    Parser.parse(program).iterator
  ).interpret
}
