package chocolate

sealed trait AST
case class Oper(name: String) extends AST
case class NumberLiteral(value: String) extends AST
case class StringLiteral(value: String) extends AST
case class MonadicModified(ast: AST, modifier: String) extends AST
case class DyadicModified(ast1: AST, ast2: AST, modifier: String) extends AST
case class WhiteSpace() extends AST
case class CloseChar() extends AST
case class MapLam(asts: Seq[AST]) extends AST
case class SeqBuild(asts: Seq[AST]) extends AST
case class Const(value: Any) extends AST
