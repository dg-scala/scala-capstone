package observatory

import akka.actor.Terminated

import scala.concurrent.Future

object Main extends App {

  simulatePlayground()

  def simulatePlayground(): Future[Terminated] = {
    import akka.stream._
    import akka.stream.scaladsl._

    import akka.{ NotUsed, Done }
    import akka.actor.ActorSystem
    import akka.util.ByteString
    import scala.concurrent._
    import scala.concurrent.duration._
    import java.nio.file.Paths

    implicit val system = ActorSystem("QuickStart")
    implicit val materializer = ActorMaterializer()

    val source: Source[Int, NotUsed] = Source(1 to 100)
    source.runForeach(i => println(i))(materializer)

    system.terminate()
  }
}
