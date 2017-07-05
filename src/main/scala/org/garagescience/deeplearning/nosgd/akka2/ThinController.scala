package org.garagescience.deeplearning.nosgd.akka2

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import org.apache.spark.ml.linalg.Matrix
import org.garagescience.deeplearning.nosgd._

import scala.collection.immutable.{Seq => TSeq}

abstract class ThinController(protected val epsilon: Double) extends Actor {

  protected[this] var count = 0
  protected[this] val log = Logging(context.system, this)

  protected def incrementAndPrint {
    count += 1; log.info(s"### ${self.path} $count")
  }

  protected def getToleranceOfList(xs: TSeq[Double]) = errorTerm(xs) <= epsilon

  protected def getBestTolerance(xs: TSeq[Double]) = getMinimum(xs) <= epsilon

  protected def errorTerm(xs: TSeq[Double]) = xs.map(e => Math.abs(e)).sum

  protected def killEverything(): Unit = context.system.terminate()

  protected def getMinimum(xs: TSeq[Double]) = xs.min

}