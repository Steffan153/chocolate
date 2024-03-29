package chocolate

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

  private def isNilad(ast: AST) = {
    ast match {
      case StringLiteral(_) => true
      case NumberLiteral(_) => true
      case Const(_) => true
      case Oper(x) => Operators.getOperator(x).arity == 0
      case _ => false
    }
  }

  private def parseAST(): AST =
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
        next() match {
          case '#' =>
            while (nonEmpty && peek != '\n')
              next()
            WhiteSpace()
          case '{' =>
            var depth = 1
            while (nonEmpty && depth > 0) {
              val c = next()
              if (c == '#' && nonEmpty && next() == '{')
                depth += 1
              if (c == '}' && nonEmpty && next() == '#')
                depth -= 1
            }
            WhiteSpace()
          case c => Oper("#" + c.toString)
        }
      case 'c' => Oper("c" + next().toString)
      case '[' =>
        var asts = Seq[AST]()
        breakable {
          while (nonEmpty) {
            parseAST() match
              case WhiteSpace() =>
              case CloseChar() => break
              case x => asts = asts :+ x
          }
        }
        SeqBuild(asts)
      case '}' => CloseChar()
      case 'γ' =>
        var asts = Seq[AST]()
        breakable {
          while (nonEmpty) {
            parseAST() match {
              case CloseChar() => break
              case WhiteSpace() =>
              case x => asts = asts :+ x
            }
          }
        }
        MapLam(asts)
      case x if Modifiers.tetradicModifiers.contains(x.toString) =>
        while (peek.isWhitespace)
          next()
        val ast1 = parseAST()
        while (peek.isWhitespace)
          next()
        val ast2 = parseAST()
        while (peek.isWhitespace)
          next()
        val ast3 = parseAST()
        while (peek.isWhitespace)
          next()
        TetradicModified(ast1, ast2, ast3, parseAST(), x.toString)
      case x if Modifiers.triadicModifiers.contains(x.toString) =>
        while (peek.isWhitespace)
          next()
        val ast1 = parseAST()
        while (peek.isWhitespace)
          next()
        val ast2 = parseAST()
        while (peek.isWhitespace)
          next()
        TriadicModified(ast1, ast2, parseAST(), x.toString)
      case x if Modifiers.dyadicModifiers.contains(x.toString) =>
        while (peek.isWhitespace)
          next()
        val ast1 = parseAST()
        while (peek.isWhitespace)
          next()
        DyadicModified(ast1, parseAST(), x.toString)
      case x if Modifiers.monadicModifiers.contains(x.toString) =>
        while (peek.isWhitespace)
          next()
        MonadicModified(parseAST(), x.toString)
      case x if Modifiers.monadicGreedModifiers.contains(x.toString) =>
        while (peek.isWhitespace)
          next()
        val ast1 = parseAST()
        if (isNilad(ast1))
          MonadicGreedModified(parseAST(), Some(ast1), x.toString)
        else
          MonadicGreedModified(ast1, None, x.toString)
      case x =>
        Oper(x.toString)

  def parse =
    var asts = List[AST]()
    while (nonEmpty)
      parseAST() match
        case WhiteSpace() =>
        case ast           => asts = asts :+ ast
    asts
}

object Parser {
  def parse(program: String) = {
    Parser(program.iterator).parse
  }
}
