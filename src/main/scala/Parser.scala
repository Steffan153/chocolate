import scala.collection.{mutable => mut}
import scala.util.control.Breaks._

class Parser(private val prog: Iterator[Char]) {
  private val buf = mut.ListBuffer.empty[Char]

  private def nonEmpty = buf.nonEmpty || prog.nonEmpty

  private def peek =
    if (buf.isEmpty)
      buf += prog.next
    buf.head

  private def next() =
    if (buf.isEmpty) prog.next else buf.remove(0)

  private def parseAST() =
    next() match
      case x if x.isWhitespace => WhiteSpace()
      case '"' =>
        val str = mut.StringBuilder()
        breakable {
          while (nonEmpty)
            val c = next()
            if (c == '"')
              break
            str.append(
              if (c == '\\')
                next() match
                  case 'n' => '\n'
                  case 't' => '\t'
                  case n   => n
              else c
            )
        }
        StringLiteral(str.toString)
      case '\'' =>
        StringLiteral(next().toString)
      case x if x.isDigit || x == '.' =>
        val num = mut.StringBuilder()
        num.append(x)
        if (x != '0')
          while (nonEmpty && (peek.isDigit || peek == '.'))
            num.append(next())
        NumberLiteral(num.toString)
      case '#' =>
        Command("#" + next().toString)
      case x =>
        Command(x.toString)

  def parse =
    var asts = List[AST]()
    while (nonEmpty)
      parseAST() match
        case _: WhiteSpace =>
        case ast           => asts = asts :+ ast
    asts
}

object Parser {
  def parse(program: String) = {
    Parser(program.iterator).parse
  }
}
