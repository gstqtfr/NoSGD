package org.garagescience.deeplearning.nosgd.akka

import akka.actor.{Actor, ActorSystem, Props}
import akka.event.Logging

import language.postfixOps
import scala.concurrent.duration._
import scala.collection.immutable.Seq
import org.apache.spark.ml.linalg.{Matrices, Matrix}
import org.garagescience.deeplearning.nosgd.MatrixGerminalCentre

object TestGCActorSystem {

  import GerminaCentreActor._

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

  def main(args: Array[String]): Unit = {

    // Create the 'helloAkka' actor system
    // val system: ActorSystem = ActorSystem("helloAkka")

    val system: ActorSystem = ActorSystem("GCActorSystem")
    

  }

}