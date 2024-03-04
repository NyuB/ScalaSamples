import scala.math.abs

// DSL usage within lambda
assertWithin(TolerantNumericalPrecision(0.2)) {
  // Logic
  1.0 isEqualTo 1.0
  // But also ...
  1.0 isEqualTo 1.1
  2.5 isLessThan 2.6
}

// Raw usage with given
{ // <- A new scope restricts this given reach
  given NumericalPrecision = StrictNumericalPrecision
  // Still
  1.0 isEqualTo 1.0
  2.5 isLessThan 2.51
  // But now ...
  try {
    1.0 isEqualTo 1.1
    fail("Should have thrown")
  } catch { 
    case a: AssertionError => { expect(a.getMessage().contains("Expected 1.0 to be equal to 1.1"), "Wrong assertion error")}
    case e => fail(s"Should have thrown 'java.lang.AssertionError: Expected 1.0 to be equal to 1.1' but got $e")
  }
}

// Multiple contexts
assertWithinTwoScopes(StrictNumericalPrecision, Sensitivity(false)) {
  42.42 isEqualTo 42.42
  "ABC" isEqualTo "aBc"
}

{ // This scope could be at a test class level
  given NumericalPrecision = TolerantNumericalPrecision(0.01)
  given CaseSensitivity = Sensitivity(false)

  // These could be in various test cases
  42.0 isEqualTo 42.001
  42.0 isLessThan 42.001
  "Scala" isEqualTo "ScAlA"
  "Scala" isEqualTo "Scala"
}

// Definitions

def expect(ok: Boolean, message: =>String) =
  if !ok then throw AssertionError(message) else ()
def fail(message: String) = throw AssertionError(message)

trait NumericalPrecision:
  // Contract
  def doubleEquality(a: Double, b: Double): Boolean
  def doubleGreaterThan(a: Double, b: Double): Boolean
  def doubleLessThan(a: Double, b: Double): Boolean

  // Provides DSL
  extension(d: Double)
    final def isEqualTo(other: Double) = expect(doubleEquality(d, other), s"Expected $d to be equal to $other")
    final def isGreaterThan(other: Double) = expect(doubleGreaterThan(d, other), s"Expected $d to be greater than $other")
    final def isLessThan(other: Double) = expect(doubleLessThan(d, other), s"Expected $d to be less than $other")

object StrictNumericalPrecision extends NumericalPrecision:
  override def doubleEquality(a: Double, b: Double) = a == b
  override def doubleGreaterThan(a: Double, b: Double) = a > b
  override def doubleLessThan(a: Double, b: Double) = a < b

class TolerantNumericalPrecision(private val tolerance: Double) extends NumericalPrecision:
  override def doubleEquality(a: Double, b: Double) = abs(a - b) <= tolerance
  override def doubleGreaterThan(a: Double, b: Double) = a > b
  override def doubleLessThan(a: Double, b: Double) = a < b

def assertWithin(scope: NumericalPrecision)(assertions: (NumericalPrecision) ?=> Unit) =
  given NumericalPrecision = scope
  assertions

trait CaseSensitivity:
  val sensitive: Boolean
  extension(s: String)
    def isEqualTo(other: String): Unit =
      val ok = if sensitive then s.equals(other) else s.toLowerCase().equals(other.toLowerCase)
      expect(ok, s"Expected $s to be equal to $other")

class Sensitivity(override val sensitive: Boolean) extends CaseSensitivity

def assertWithinTwoScopes(numericalScope: NumericalPrecision, caseScope: CaseSensitivity)(assertions: (NumericalPrecision) ?=> (CaseSensitivity) ?=> Unit) =
  given n: NumericalPrecision = numericalScope
  given c: CaseSensitivity = caseScope
  assertions


