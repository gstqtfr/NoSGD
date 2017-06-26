package org.garagescience.deeplearning.nosgd.akka

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import org.apache.spark.ml.linalg.Matrix
import org.garagescience.deeplearning.nosgd._

import scala.collection.immutable.{Seq => TSeq}

class GerminalCentreController(m: Matrix,
                               gca: ActorRef,
                               iterations: Int = 100) extends Actor {

  private[this] var count = 0
  private[this] val log = Logging(context.system, this)
  private[this] def incrementAndPrint { count += 1; println(s"${self.path} $count") }
  private[this] def getToleranceOfList(xs: TSeq[Double],
                                 epsilon: Double=0.01) = errorTerm(xs) <= epsilon
  private[this] def getBestTolerance(xs: TSeq[Double],
                                     epsilon: Double=0.01) = getMinimum(xs) <= epsilon
  private[this] def errorTerm(xs: TSeq[Double]) = xs.map(e=>Math.abs(e)).sum
  private[this] def killEverything(): Unit = context.system.terminate()
  private[this] def getMinimum(xs: TSeq[Double]) = xs.min

  def receive = {

    case KickOff =>
      log.info(s"${self.path} received kickOff")
      gca ! GetUpdateGC

    case AckUpdateGC =>
      log.info(s"${self.path} received AckUpdateGC")
      incrementAndPrint
      gca ! GetErrorsGC

    case ErrorsGC(xs: TSeq[Double]) =>
      log.info(s"${self.path} received ErrorsGC")
      log.info(s"${self.path} errors: ${xs}")
      log.info(s"${self.path} error term: ${count} : ${getMinimum(xs)}")
      // could also do if (count == iterations) here ...
      if (getBestTolerance(xs)) {
        log.info(s"${self.path} getting best matrix from gca")
        gca ! GetMinimumGC
      }
      else
        gca ! GetUpdateGC


    case MinimumGC(m) =>
      log.info(s"${self.path}: best matrix is: ${m}")
      gca ! FinalWhistle
      log.info(s"${self.path} killing gca")
      log.info(s"${self.path} committing suicide")
      context.stop(gca)
      context.stop(self)
      log.info(s"${self.path} killing everything ...")
      killEverything()


    case _ =>
      log.info(s"${self.path} received unknown message from ${sender}")
  }

}

object GerminalCentreController {

  /*
  def props(matrix: Matrix, error: Matrix => Double, p: ActorRef): Props =
    Props(new GerminalCentreController(matrix, error, p, 1000))
    */

  def props(matrix: Matrix, p: ActorRef, iterations: Int): Props =
    Props(new GerminalCentreController(matrix, p, iterations))

  def props(matrix: Matrix, p: ActorRef): Props =
    Props(new GerminalCentreController(matrix, p, 1000))

}