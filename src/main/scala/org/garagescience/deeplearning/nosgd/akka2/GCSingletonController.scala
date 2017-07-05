package org.garagescience.deeplearning.nosgd.akka2

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.Logging
import org.garagescience.deeplearning.nosgd.linalg.{LinalgUtils, Matrix}

//import org.apache.spark.ml.linalg.{Matrices, Matrix}
import org.garagescience.deeplearning.nosgd.{AckUpdateGC, _}

import scala.collection.immutable.{Seq => TSeq}
import scala.language.postfixOps

class Pong extends Actor {

  private[this] val log = Logging(context.system, this)

  override def receive: Receive = {

    case GetUpdateGC =>
      log.info(s"${self.path} received GetUpdateGC")
      sender ! AckUpdateGC

    case GetErrorsGC =>
      log.info(s"${self.path} received GetErrorsGC")
      val xs: TSeq[Double] = for (i <- 0 until 5) yield scala.util.Random.nextGaussian()
      sender ! ErrorsGC(xs)

    case FinalWhistle =>
      log.info(s"${self.path} received FinalWhistle, shutting down")
      context.stop(self)

    case _ =>
      log.info(s"${self.path} received unknown message")
  }
}

object Pong {

  // best practise is to put the props close to where the
  // actor itself is init'd
  def props: Props = Props[Pong]

}

object GCSingletonController {

  /*
  private def randomMatrix(rows: Int, cols: Int, sz: Int) = {
    val tmpArray = for {i <- 0 until sz} yield scala.util.Random.nextGaussian
    new _DenseMatrix(rows, cols, tmpArray.toArray)
  }


  private final val target = new _DenseMatrix(3, 3,
    Array(
      0.0, 0.0, 0.0,
      0.0, 0.0, 0.0,
      0.0, 0.0, 0.0
    )
  )
  */

  private final val target = LinalgUtils.zeroMatrix(3, 3, 3 * 3)


  private def error(m1: Matrix[Double]): Double =
    Math.sqrt(
      Math.pow(Math.abs((for {i <- 0 until target.width
                              j <- 0 until target.height}
        yield target(i, j) - m1(i, j)).sum), 2.0)
    )


  def main(args: Array[String]): Unit = {

    val system = ActorSystem("TestGCActorSytem")

    def init = LinalgUtils.randomMatrix(
      target.width,
      target.height,
      target.toArray.length)

    val gca: ActorRef = system.
      actorOf(GerminalCentreActor.props(init, error),
        name = "gca")

    val gcc: ActorRef = system.
      actorOf(GerminalCentreSingletonController.props(target, gca),
        name = "gcc")

    gcc ! KickOff


  }
}