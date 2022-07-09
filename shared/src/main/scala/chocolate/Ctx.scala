package chocolate

class Ctx {
  var inputs: Seq[Any] = Seq()
  var inputCycle: Iterator[Any] = Iterator()
  var contextVar: Option[Any] = None

  def copy = {
    val c = Ctx()
    c.inputs = inputs
    c.inputCycle = inputCycle
    c.contextVar = contextVar
    c
  }
}