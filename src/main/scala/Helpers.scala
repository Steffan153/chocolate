import scala.collection.mutable
import scala.annotation.nowarn

def chocPrint(s: Any) = {
  @nowarn
  lazy val res: (Any, Boolean, Boolean) => Any = {
    case (s: String, quotify: Boolean, _) => if (quotify) s"\"${s.replace("\\", "\\\\").replace("\n", "\\n").replace("\t", "\\t").replace("\"", "\\\"")}\"" else s
    case (s: spire.math.Number, _, _) => s
    case (s: LazyList[Any], _, forceEval: Boolean) =>
      if (forceEval)
        "[" + s.mkString(", ") + "]"
      else () => {
        print("[")
        s.dropRight(1).foreach { x =>
          print(res(x, true, true))
          print(", ")
        }
        print(res(s.last, true, true))
        println("]")
      }
    case (s: Seq[Any], _, _) => "[" + s.map(res(_, true, true)).mkString(", ") + "]"
  }
  res(s, true, false) match {
    case (x: (() => Any)) => x()
    case x => println(x)
  }
}
