trait ErrorHandling:
  type Result[T]
  def error[T](reason: String): Result[T]
  def success[T](t: T): Result[T]
  extension[T](r: Result[T])
    def map[A](f: T => A): Result[A]
    def flatMap[A](f: T => Result[A]): Result[A]

object HandleByOption extends ErrorHandling:
  type Result[T] = Option[T]
  def error[T](reason: String): Result[T] = None
  def success[T](t: T) = Some(t)
  extension[T](r: Result[T])
    def map[A](f: T => A): Result[A] = r.asInstanceOf[Option[T]].map(f)
    def flatMap[A](f: T => Result[A]): Result[A] = r.asInstanceOf[Option[T]].flatMap(f)

object HandleByException extends ErrorHandling:
  type Result[T] = T
  def success[T](t: T): Result[T] = t
  def error[T](reason: String): Result[T] = throw AssertionError(reason)
  extension[T](r: Result[T])
    def map[A](f: T => A): Result[A] = f(r)
    def flatMap[A](f: T => Result[A]): Result[A] = f(r)

def operationOk(using err: ErrorHandling): err.Result[String] = err.success("Youpi")
def operationKo(using err: ErrorHandling): err.Result[String] = err.error("Huh oh")

{
  given ErrorHandling = HandleByException
  val s: String = operationOk
  println(s"Got $s yeah !")
  
  // Would throw
  // val e = operationKo
}

{
  given ErrorHandling = HandleByOption
  val s: Option[String] = operationOk // Dependent return type
  println(s"Maybe got $s")
  val e = operationKo
  println(s"Maybe err $e")
}
