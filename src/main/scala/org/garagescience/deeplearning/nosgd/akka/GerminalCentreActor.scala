package org.garagescience.deeplearning.nosgd.akka

import akka.actor.{Actor, Props}
import akka.event.Logging
import org.garagescience.deeplearning.nosgd.linalg._
import org.garagescience.deeplearning.nosgd._
import scala.language.postfixOps

// TODO: type parameterise this code!!! Matrix=>T? LinalgMatrixGerminalCentre=>T?

class GerminalCentreActor(m: Matrix[Double],
                          error: Matrix[Double] => Double) extends Actor {

  // TODO: this'll be problematic for type param, unless we pass it in
  // TODO: as a param to the ctor ...
  private val gc: MatrixGerminalCentre = new MatrixGerminalCentre(m)
  private val log = Logging(context.system, this)

  def receive = {

    case GetUpdateGC =>
      log.info(s"${self.path} received UpdateGC")
      gc.update(error)
      sender ! AckUpdateGC

    case GetErrorsGC =>
      log.info(s"${self.path} received ErrorsGC")
      // getFittestClones(xs: Array[Matrix[Double]],
      //                  f: Matrix[Double] => Double): Array[Double]
      val errors: Array[Double] = gc.getClonePoolFitness(error)
      log.info(s"${self.path} errors: $errors")
      sender ! ErrorsGC(errors)

    case GetMinimumGC =>
      log.info(s"${self.path} received GetMinimumGC")
      val _m = gc.getFittest(error).head
      sender ! MinimumGC(_m)


    case FinalWhistle =>
      log.info(s"${self.path} received FinalWhistle, shutting down")
      context.stop(self)

    case _ =>
      log.info(s"${self.path} received unknown message from ${sender}")
  }
}

object GerminalCentreActor {

  // best practise is to put the props close to where the
  // actor itself is init'd
  def props(m: Matrix[Double], error: Matrix[Double] => Double): Props =
    Props(new GerminalCentreActor(m, error))

}
