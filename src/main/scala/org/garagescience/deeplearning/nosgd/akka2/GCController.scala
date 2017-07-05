package org.garagescience.deeplearning.nosgd.akka2

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
//import org.apache.spark.ml.linalg.Matrix
import org.garagescience.deeplearning.nosgd._
import org.garagescience.deeplearning.nosgd.linalg._
import scala.collection.immutable.{Seq => TSeq}

class GCController(m: Matrix[Double],
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
      log.info(s"${self.path} ${count} received ErrorsGC")
      log.info(s"${self.path} ${count} errors: ${xs}")
      log.info(s"${self.path} ${count} error term: ${count} : ${getMinimum(xs)}")
      // could also do if (count == iterations) here ...
      if (getBestTolerance(xs)) {
        log.info(s"${self.path} ${count} getting best matrix from gca")
        sender ! GetMinimumGC
      }
      else
        sender ! GetUpdateGC


    case MinimumGC(m) =>
      log.info(s"${self.path}: ${count} best matrix is:\n${m}")
      sendToAll(FinalWhistle, gcl)
      log.info(s"${self.path} ${count} killing actors")
      //killAllTheOthers(gcl)
      log.info(s"${self.path} ${count} killing everything! kill! kill! kill!!! ...")
      killEverything()
      log.info(s"${self.path} ${count} committing suicide - goodbye cruel world ...")
      context.stop(self)



    case _ =>
      log.info(s"${self.path} received unknown message from ${sender}")

  }

}

object GCController {

  def props(matrix: Matrix[Double], l: List[ActorRef]): Props =
    Props(new GCController(matrix, l))

  def props(matrix: Matrix[Double], l: List[ActorRef], e: Double): Props =
    Props(new GCController(matrix, l, e))

}