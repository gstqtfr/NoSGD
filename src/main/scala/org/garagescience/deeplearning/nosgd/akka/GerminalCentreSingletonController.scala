package org.garagescience.deeplearning.nosgd.akka

import akka.actor.{Actor, ActorRef, Props}
import org.garagescience.deeplearning.nosgd._
import org.garagescience.deeplearning.nosgd.linalg._
import scala.collection.immutable.{Seq => TSeq}

class GerminalCentreSingletonController[T](m: Matrix[T],
                                        gca: ActorRef,
                                        iterations: Int = 100,
                                        epsilon: Double = 0.01) extends ThinController(epsilon) {

  def receive = {

    case KickOff =>
      log.info(s"${self.path} received kickOff")
      gca ! GetUpdateGC

    case AckUpdateGC =>
      log.info(s"${self.path} received AckUpdateGC")
      incrementAndPrint
      gca ! GetErrorsGC

    case ErrorsGC(xs: Array[Double]) =>
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
      log.info(s"${self.path} killing everything! kill! kill! kill!!! ...")
      killEverything()


    case _ =>
      log.info(s"${self.path} received unknown message from ${sender}")
  }

}

object GerminalCentreSingletonController {

  /*
  def props(matrix: Matrix, error: Matrix => Double, p: ActorRef): Props =
    Props(new GerminalCentreController(matrix, error, p, 1000))
    */

  def props[T](matrix: Matrix[T], p: ActorRef, iterations: Int): Props =
    Props(new GerminalCentreSingletonController(matrix, p, iterations))

  def props[T](matrix: Matrix[T], p: ActorRef): Props =
    Props(new GerminalCentreSingletonController(matrix, p, 1000))

}