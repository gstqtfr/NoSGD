package org.garagescience.deeplearning.nosgd.actors

import akka.actor.{Actor, ActorRef, Props}
import org.garagescience.deeplearning.nosgd.linalg._
import org.garagescience.deeplearning.nosgd.mlp.training.SomHypeTrainer


// TODO: okay, whatever the other decisions, this HAS to be an Array[Double]
// TODO: let's make this a SomHypeNeuralNetwork, or SomHypeTrainer

class GCController(trainer: SomHypeTrainer,
                    //m: Array[Double],
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


    //case ErrorsGC(xs: TSeq[Double]) =>
    case ErrorsGC(xs: Array[Double]) =>
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
      // TODO: need to make this dependent on lowest-error (> epsilon) or
      // TODO: mak iterations or something
      sendToAll(FinalWhistle, gcl)
      log.info(s"${self.path} ${count} killing actors")
      //killAllTheOthers(gcl)
      log.info(s"${self.path} ${count} killing everything! kill! kill! kill!!! ...")
      killEverything()
      log.info(s"${self.path} ${count} committing suicide - goodbye cruel world ...")
      context.stop(self)



    case m =>
      log.info(s"${self.path} received unknown message from ${sender}: ${m}")

  }

}

object GCController {

  def props(network: SomHypeTrainer, l: List[ActorRef]): Props =
    Props(new GCController(network, l))

  def props(network: SomHypeTrainer, l: List[ActorRef], e: Double): Props =
    Props(new GCController(network, l, e))

}