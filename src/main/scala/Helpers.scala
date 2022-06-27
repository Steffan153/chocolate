import scala.collection.mutable

def chocPrint(s: Any) = {
  lazy val res: (Any, Any) => Any = {
    case (s: String, quotify: Boolean) => if (quotify) s"\"${s.replace("\\", "\\\\").replace("\n", "\\n").replace("\t", "\\t").replace("\"", "\\\"")}\"" else s
    case (s: spire.math.Number, _) => s
    case (s: Seq[Any], _) => "[" + s.map(res(_, true)).mkString(", ") + "]"
  }
  println(res(s, false))
}
