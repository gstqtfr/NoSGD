package org.garagescience.deeplearning.nosgd.akka

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import org.apache.spark.ml.linalg.Matrix
import org.garagescience.deeplearning.nosgd.{FinalWhistle, GetErrorsGC, GetUpdateGC}
import org.garagescience.deeplearning.nosgd.{AckUpdateGC, ErrorsGC, KickOff}

import scala.collection.immutable.{Seq => TSeq}

class GerminalCentreController(m: Matrix,
                               error: Matrix => Double,
                               gca: ActorRef,
                               iterations: Int = 100) extends Actor {

  private[this] var count = 0
  private[this] val log = Logging(context.system, this)
  private[this] def incrementAndPrint { count += 1; println(s"${self.path} $count") }

  private[this] def getTolerance(xs: TSeq[Double],
                                 epsilon: Double=0.1) = Math.abs(xs.sum) >= epsilon

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
      log.info(s"$self.path} errors: xs")
      // TODO: how do we terminate?!?!
      if (getTolerance(xs)) {
      //if (count == iterations) {
        gca ! FinalWhistle
        log.info(s"${self.path} killing gca")
        log.info(s"${self.path} committing suicide")
        context.stop(gca)
        context.stop(self)
      }
      else
        gca ! GetUpdateGC

    case _ =>
      log.info(s"${self.path} received unknown message from ${sender}")
  }

}

object GerminalCentreController {

  def props(matrix: Matrix, error: Matrix => Double, p: ActorRef): Props =
    Props(new GerminalCentreController(matrix, error, p, 100))
}