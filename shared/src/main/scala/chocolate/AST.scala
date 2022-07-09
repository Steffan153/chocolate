package chocolate

sealed trait AST
case class Oper(name: String) extends AST
case class NumberLiteral(value: String) extends AST
case class StringLiteral(value: String) extends AST
case class Ref(ast: AST) extends AST
case class WhiteSpace() extends AST
case class CloseChar() extends AST
case class Lam(asts: Seq[AST]) extends AST
case class MapLam(asts: Seq[AST]) extends AST