/** Mark a class as usable for injection */
trait Provided[A]

/** Provides the actual implementation for DI */
trait Provide[A]:
  def get: A

/** Inject an unique instance */
class Singleton[A](private val instance: A) extends Provide[A]:
  override def get = instance

/** Inject a new evaluation of an expression for each injection */
class Factory[A](expr: =>A) extends Provide[A]:
  override def get = expr

/** DI Resolution */ 
given[A: Provided](using provide: Provide[A]): A = provide.get


// Usage Example

enum AuthLevel:
  case Admin, User, Guest
  def includes(other: AuthLevel) = (this, other) match
    case (Admin, _) => true
    case (User, Admin) => false
    case (User, _) => true
    case (Guest, Guest) => true
    case _ => false

case class Event(visibilityLevel: AuthLevel, message: String)

class Database:
  val entries: List[Event] = List(
    Event(AuthLevel.Admin, "Sensitive information"),
    Event(AuthLevel.User, "Private info"),
    Event(AuthLevel.Guest, "Anonymous but public"),
  )
  def findAll(using level: AuthLevel) = entries.filter(e => level.includes(e.visibilityLevel))

class CurrentRequest(val requestId: Long, val authLevel: AuthLevel)

given levelFromRequest(using request: CurrentRequest): AuthLevel = request.authLevel
def getNormalizedMessage(using db: Database, req: CurrentRequest) =
  db.findAll.map(_.message.toLowerCase)

def handleRequest(using db: Database)(req: CurrentRequest) =
  given CurrentRequest = req
  getNormalizedMessage

given Provided[Database]()
given Provide[Database] = Singleton(Database())


handleRequest(CurrentRequest(0, AuthLevel.Admin))
handleRequest(CurrentRequest(1, AuthLevel.User))
handleRequest(CurrentRequest(2, AuthLevel.Guest))
