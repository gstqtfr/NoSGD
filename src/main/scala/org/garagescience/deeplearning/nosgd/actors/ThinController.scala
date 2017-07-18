package org.garagescience.deeplearning.nosgd.actors


import org.garagescience.deeplearning.nosgd._
import akka.event.Logging
import akka.actor.{Actor, ActorRef, Props}


// TODO: perhaps we need another controller, which takes a maximum number of iterations
// TODO: in addition to/instead of a tolerance?

abstract class ThinController(protected val epsilon: Double) extends Actor {

  protected[this] var count = 0
  protected[this] val log = Logging(context.system, this)

  protected def incrementAndPrint() {
    count += 1; log.info(s"### ${self.path} $count")
  }

  protected def getToleranceOfList(xs: Array[Double]): Boolean = errorTerm(xs) <= epsilon

  protected def getBestTolerance(xs: Array[Double]): Boolean = getMinimum(xs) <= epsilon

  protected def errorTerm(xs: Array[Double]): Double = xs.map(e => Math.abs(e)).sum

  protected def killEverything(): Unit = context.system.terminate()

  protected def getMean(xs: Array[Double]): Double = xs.sum / xs.length

  protected def getMinimum(xs: Array[Double]): Double = xs.min

}