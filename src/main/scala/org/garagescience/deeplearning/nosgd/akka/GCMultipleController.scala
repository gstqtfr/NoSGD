package org.garagescience.deeplearning.nosgd.akka

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
// import org.garagescience.deeplearning.nosgd.linalg.Vector.At
import scala.collection.immutable.{IndexedSeq, Seq=>TSeq}
import org.garagescience.deeplearning.nosgd.linalg._
import org.garagescience.deeplearning.nosgd.{AckUpdateGC, _}
import scala.collection.immutable.{Seq => TSeq}
import scala.language.postfixOps
// import scala.util.Random

object GCMultipleController {

  private val r = new scala.util.Random

  val rows = 3
  val cols = 3
  private final val target = LinalgUtils.zeroMatrix(3,3,3*3)

  private def error(m1: Matrix[Double]): Double =
    Math.sqrt(
      Math.pow(Math.abs((for {i <- 0 until target.width
                              j <- 0 until target.height}
        yield target(i,j) - m1(i,j)).sum), 2.0)
    )

  private def creatActor(sys: ActorSystem,
                         _init: () => Matrix[Double],
                        _error: Matrix[Double] => Double): ActorRef = {
    sys.actorOf(GerminalCentreActor.props(_init(), _error))
  }

  private final val popSz = 10
  private final val epsilon = 0.00000001

  def main(args: Array[String]): Unit = {

    def init() = LinalgUtils.randomMatrix(
      target.width,
      target.height,
      target.toArray.length)

    val system = ActorSystem("TestGCmultipleActorSytem")

    val gcl: List[ActorRef] =
      {for {i <- 0 until popSz} yield creatActor(system, init, error)}.toList

    val gcc: ActorRef = system.
      actorOf(GCController.props(target, gcl, epsilon),
        name = "gcl")


    gcc ! KickOff

  }

}
