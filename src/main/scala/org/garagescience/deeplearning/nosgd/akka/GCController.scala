package org.garagescience.deeplearning.nosgd.akka

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import org.apache.spark.ml.linalg.Matrix
import org.garagescience.deeplearning.nosgd._
import scala.collection.immutable.{Seq => TSeq}

class GCController(m: Matrix,
                  // a supervisor actor would create its own, rather than
                  // have it passed
                   gcl: List[ActorRef],
                   epsilon: Double = 0.01,
                   iterations: Int = 100
                   ) extends ThinController(epsilon) {


  def sendToAll(msg: GCMessages, actor: List[ActorRef]) = {

    def _sendToAll(_msg: GCMessages, _actor: List[ActorRef]): List[ActorRef] = _actor match {
      case Nil => _actor
      case h :: t =>
        h ! _msg
        _sendToAll(_msg, t)

    }

    _sendToAll(msg, actor)
  }


  def killAllTheOthers(actor: List[ActorRef]) = {

    def _killAllTheOthers(_actor: List[ActorRef]): List[ActorRef] = _actor match {
      case Nil => _actor
      case h :: t =>
        context.stop(h)
        _killAllTheOthers(t)
    }

    _killAllTheOthers(actor)
  }

  def receive = {

    case KickOff =>
      log.info(s"${self.path} received kickOff")
      // tell all our actors to update their germminal centres
      sendToAll(GetUpdateGC, gcl)


    case AckUpdateGC =>
      log.info(s"${self.path} received AckUpdateGC")
      incrementAndPrint
      sender ! GetErrorsGC


    case ErrorsGC(xs: TSeq[Double]) =>
      log.info(s"${self.path} received ErrorsGC")
      log.info(s"${self.path} errors: ${xs}")
      log.info(s"${self.path} error term: ${count} : ${getMinimum(xs)}")
      // could also do if (count == iterations) here ...
      if (getBestTolerance(xs)) {
        log.info(s"${self.path} getting best matrix from gca")
        sender ! GetMinimumGC
      }
      else
        sender ! GetUpdateGC


    case MinimumGC(m) =>
      log.info(s"${self.path}: best matrix is: ${m}")
      sender ! FinalWhistle
      log.info(s"${self.path} killing gca")
      log.info(s"${self.path} committing suicide")
      // TODO: need a way to kill everything here ...
      //context.stop(gca)
      killAllTheOthers(gcl)
      context.stop(self)
      log.info(s"${self.path} killing everything! kill! kill! kill!!! ...")
      killEverything()


    case _ =>
      log.info(s"${self.path} received unknown message from ${sender}")

  }


}