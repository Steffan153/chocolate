package chocolate

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportTopLevel, JSExport}
import scala.scalajs.js.JSConverters._

@JSExportTopLevel("Chocolate")
object JSChocolate {
  @JSExport
  def execute(program: String, inputs: js.Array[String]) = {
    lazy val conv: Any => Any = {
      case (x: LazyList[Any]) => x.iterator.toJSIterator
      case (x: Seq[Any]) => x.map(conv).toJSArray
      case x => x
    }
    conv(Interpreter.interpret(program, inputs.toSeq))
  }
}