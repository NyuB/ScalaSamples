import scala.language.experimental.genericNumberLiterals
import scala.util.FromDigits

@main def main() =
  val a = 1e-5.mm
  val b = Nanometer(10)
  val c: Nanometer = 10
  val d = 10.nm
  val e = -4.fm
  println(s"$a | $b | $c | $d | $e")

/** The reference unit */
sealed class Nanometer(val value: Double):
  override def toString = s"$value (nm)"
object Nanometer:
  given FromDigits[Nanometer] with
    def fromDigits(s: String) = Nanometer(s.toDouble)

final class Millimeter(v: Double) extends Nanometer(v * 1e6)
object Millimeter:
  given FromDigits[Millimeter] with
    def fromDigits(s: String) = Millimeter(s.toDouble)

final class Femtometer(v: Double) extends Nanometer(v * 1e-6)
object Femtometer:
  given FromDigits[Femtometer] with
    def fromDigits(s: String) = Femtometer(s.toDouble)


extension(d: Double)
  def nm = Nanometer(d)
  def mm = Millimeter(d)
  def fm = Femtometer(d)

extension(i: Int)
  def nm = Nanometer(i.toDouble)
  def mm = Millimeter(i.toDouble)
  def fm = Femtometer(i.toDouble)
