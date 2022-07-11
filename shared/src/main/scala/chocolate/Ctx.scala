package chocolate

class Ctx {
  var inputs: Seq[Any] = Seq.empty
  var inputCycle: Iterator[Any] = Iterator.empty
  var contextVars: Iterator[Any] = Iterator.empty
  var contextVarsSeq: Seq[Any] = Seq.empty

  def copy = {
    val c = Ctx()
    c.inputs = inputs
    c.inputCycle = inputCycle
    c.contextVars = contextVars
    c
  }
}