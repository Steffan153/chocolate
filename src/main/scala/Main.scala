import scala.io.Source
@main def chocolate(fileName: String) = {
  Interpreter.interpret(Source.fromFile(fileName).mkString)
}