package chocolate

class Ctx {
  var inputs: Seq[Any] = Seq()
  var inputCycle: Iterator[Any] = Iterator()
}