package org.garagescience.deeplearning.nosgd.akka

import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging
import language.postfixOps
import scala.concurrent.duration._
import scala.collection.immutable.Seq
import org.apache.spark.ml.linalg.{Matrices, Matrix}
import org.garagescience.deeplearning.nosgd.MatrixGerminalCentre

// TODO: move these when we have all the messages

sealed trait GCMessages
final case object UpdateGC extends GCMessages
final case object ErrorsGC extends GCMessages


// TODO: here's what we need the actor system to do:
// TODO: update, applying the error function
// TODO: get & return the errors

class GerminaCentreActor(m: Matrix, error: Matrix => Double) extends Actor {

  private val gc = new MatrixGerminalCentre(m)
  private val log = Logging(context.system, this)

  def receive = {
    case UpdateGC =>
      log.info(s"${self.path} received UpdateGC")
      gc.update(error)

    case ErrorsGC =>
      log.info(s"${self.path} received ErrorsGC")
      // TODO: send this to the caller (return to sender!)
      val errors = gc.clones.map(xs => error(xs))
      sender ! errors

    case _ =>
      log.info(s"${self.path} received unknown message")
  }
}

object GerminaCentreActor {


  def props(m: Matrix, error: Matrix => Double): Props =
    Props(new GerminaCentreActor(m, error))

}