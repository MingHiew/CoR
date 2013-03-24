case class Event(level: Int, title: String)

//Base handler class
abstract class Handler {
  val successor: Option[Handler]
  def handleEvent(event: Event): Unit
}

//Customer service agent
class Agent(val successor: Option[Handler]) extends Handler {
  override def handleEvent(event: Event): Unit = {
    event match {
      case e if e.level < 2 => println("CS Agent Handled event: " + e.title)
      case e if e.level > 1 => {
        successor match {
          case Some(h: Handler) => h.handleEvent(e)
          case None => println("Agent: This event cannot be handled.")
        }
      }
    }
  }
}

class Supervisor(val successor: Option[Handler]) extends Handler {
  override def handleEvent(event: Event): Unit = {
    event match {
      case e if e.level < 3 => println("Supervisor handled event: " + e.title)
      case e if e.level > 2 => {
        successor match {
          case Some(h: Handler) => h.handleEvent(e)
          case None => println("Supervisor: This event cannot be handled")
        }
      }
    }
  }
}

class Boss(val successor: Option[Handler]) extends Handler {
  override def handleEvent(event: Event): Unit = {
    event match {
      case e if e.level < 4 => println("Boss handled event: " + e.title)
      case e if e.level > 3 => successor match {
        case Some(h: Handler) => h.handleEvent(e)
        case None => println("Boss: This event cannot be handled")
      }
    }
  }
}

object Main {
  def main(args: Array[String]) {
    val boss = new Boss(None)
    val supervisor = new Supervisor(Some(boss))
    val agent = new Agent(Some(supervisor))
    
    println("Passing events")
    val events = Array(
      Event(1, "Technical support"), 
      Event(2, "Billing query"),
      Event(1, "Product information query"), 
      Event(3, "Bug report"), 
      Event(5, "Police subpoena"), 
      Event(2, "Enterprise client request")
    )
    events foreach { e: Event =>
      agent.handleEvent(e)
    }
  }
}
