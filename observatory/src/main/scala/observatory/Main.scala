package observatory

import akka.actor.Terminated
import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import observatory.Visualization.visualize

import scala.concurrent.Future

object Main extends App {

  def imagine(): Unit = {
    val tempColors = Seq(
      (60.0, Color(255, 255, 255)),
      (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)),
      (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)),
      (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)),
      (-60.0, Color(0, 0, 0))
    )

    val img = visualize(locationYearlyAverageRecords(locateTemperatures(1975, "/stations.csv", "/1975.csv")), tempColors)
    img.output(new java.io.File("/home/dragan/Desktop/imagine1975.png"))
  }

//  imagine()

//  simulatePlayground()

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