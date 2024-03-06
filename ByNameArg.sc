@main def main() =
  val a = byValue(sideEffectDouble(1.0)) // logged one
  val b = byName(sideEffectDouble(2.0)) // logged twice
  val c = byInline(sideEffectDouble(3.0)) // logged twice

def sideEffectDouble(d: Double) =
  println(s"Log $d")
  d

def byValue(d: Double) = d + d
def byName(d: => Double) = d + d
inline def byInline(inline d: Double) = d + d
