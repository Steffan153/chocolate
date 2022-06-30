package chocolate

import scala.io.Source

object JVMMain {
  def main(args: Array[String]) = {
    Interpreter.interpret(Source.fromFile(args(0)).mkString, args.drop(1).toSeq).foreach(println)
  }
}