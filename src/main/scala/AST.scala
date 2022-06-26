sealed trait AST
case class Command(name: String) extends AST
case class NumberLiteral(value: String) extends AST