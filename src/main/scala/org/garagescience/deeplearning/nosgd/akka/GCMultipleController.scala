package org.garagescience.deeplearning.nosgd.akka

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.Logging
import org.apache.spark.ml.linalg.{Matrices, Matrix}
import org.garagescience.deeplearning.nosgd.{AckUpdateGC, _}
import scala.collection.immutable.{Seq => TSeq}
import scala.language.postfixOps
import scala.util.Random

object GCMultipleController {

  private val r = new scala.util.Random

  private def randomMatrix(rows: Int, cols: Int, sz: Int) = {
    val tmpArray = for {i <- 0 until sz} yield r.nextGaussian
    Matrices.dense(rows, cols, tmpArray.toArray)
  }

  private final val target = Matrices.dense(3, 3,
    Array(
      0.0, 0.0, 0.0,
      0.0, 0.0, 0.0,
      0.0, 0.0, 0.0
    )
  )

  private def error(m1: Matrix): Double =
    Math.sqrt(
      Math.pow(Math.abs((for {i <- 0 until target.numRows
                              j <- 0 until target.numCols}
        yield target(i,j) - m1(i,j)).sum), 2.0)
    )

  private def creatActor(sys: ActorSystem,
                         _init: () => Matrix,
                        _error: Matrix => Double): ActorRef = {
    sys.actorOf(GerminalCentreActor.props(_init(), _error))
  }

  private final val popSz = 10
  private final val epsilon = 0.00000001

  def main(args: Array[String]): Unit = {

    def init() = randomMatrix(
      target.numRows,
      target.numCols,
      target.toArray.length)

    val system = ActorSystem("TestGCmultipleActorSytem")

    val gcl: List[ActorRef] = {for {i <- 0 until popSz} yield creatActor(system, init, error)}.toList

    val gcc: ActorRef = system.
      actorOf(GCController.props(target, gcl, epsilon),
        name = "gcl")


    gcc ! KickOff

  }

}
