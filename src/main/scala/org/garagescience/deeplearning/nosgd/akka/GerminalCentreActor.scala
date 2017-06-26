package org.garagescience.deeplearning.nosgd.akka

import akka.actor.{Actor, Props}
import akka.event.Logging
import org.apache.spark.ml.linalg.Matrix
import org.garagescience.deeplearning.nosgd._

import scala.collection.immutable.Seq
import scala.language.postfixOps

// TODO: type parameterise this code!!! Matrix=>T

class GerminaCentreActor(m: Matrix, error: Matrix => Double) extends Actor {

  // TODO: this'll be problematic for type param, unless we pass it in
  // TODO: as a param to the ctor ...
  private val gc = new MatrixGerminalCentre(m)
  private val log = Logging(context.system, this)

  def receive = {

    case GetUpdateGC =>
      log.info(s"${self.path} received UpdateGC")
      gc.update(error)
      sender ! AckUpdateGC

    case GetErrorsGC =>
      log.info(s"${self.path} received ErrorsGC")
      val errors: Seq[Double] = gc.clones.map(xs => error(xs))
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

object GerminaCentreActor {

  // best practise is to put the props close to where the
  // actor itself is init'd
  def props(m: Matrix, error: Matrix => Double): Props =
    Props(new GerminaCentreActor(m, error))

}
