package observatory

import akka.actor.Terminated

import scala.concurrent.Future

object Main extends App {

  simulatePlayground()

  def simulatePlayground(): Future[Terminated] = {
    import akka.stream._
    import akka.stream.scaladsl._

    import akka.{NotUsed, Done}
    import akka.actor.ActorSystem
    import akka.util.ByteString
    import scala.concurrent._
    import scala.concurrent.duration._
    import java.nio.file.Paths

    implicit val system = ActorSystem("QuickStart")
    implicit val materializer = ActorMaterializer()

    val source: Source[Int, NotUsed] = Source(1 to 100)
        source.runForeach(i => println(i))(materializer)
    val factorials = source.scan(BigInt(1))((acc, next) => acc * next)

    val result: Future[IOResult] =
      factorials
        .map(num => ByteString(s"$num\n"))
        .runWith(FileIO.toPath(Paths.get("factorials.txt")))

    Await.result(result, 5.seconds)

    def chrottle: Future[Done] =
      factorials
        .zipWith(source)((num, idx) => s"$idx! = $num")
        .throttle(1, 100.millis, 1, ThrottleMode.shaping)
        .runForeach(println)

    Await.result(chrottle, 11.seconds)

    system.terminate()
  }

}