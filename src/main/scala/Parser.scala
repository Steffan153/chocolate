import scala.collection.{mutable => mut}
class Parser(private val prog: Iterator[Char]) {
  private val buf = mut.ListBuffer.empty[Char]
  private def nonEmpty = buf.nonEmpty || prog.nonEmpty
  private def peek = {
    if (buf.isEmpty) {
      buf += prog.next
    }
    buf.head
  }
  private def next() = {
    if (buf.isEmpty) prog.next else buf.remove(0)
  }
  private def parseAST() = {
    next() match {
      case x if x.isDigit || x == '.' => {
        val num = mut.StringBuilder()
        num.append(x)
        while (nonEmpty && (peek.isDigit || peek == '.')) {
          num.append(next())
        }
        NumberLiteral(num.toString())
      }
      case x => {
        Command(x.toString())
      }
    }
  }
  def parse = {
    var asts = List[AST]()
    while (nonEmpty) {
      asts = asts :+ parseAST()
    }
    asts
  }
}

object Parser {
  def parse(program: String) = {
    Parser(program.iterator).parse
  }
}
