package org.garagescience.deeplearning.nosgd.actors

import akka.actor.{Actor, Props}
import org.garagescience.deeplearning.nosgd.mlp._
import org.garagescience.deeplearning.nosgd.mlp.NeuralNetwork._
import scala.language.postfixOps
import scala.collection.immutable.{Seq => TSeq}

// TODO: REALLY need to pass in the interface here, then we have just
// TODO: the one class to do all this stuff ...

class FixedPointNetActor(numInputs: Int,
                         numOutputs: Int,
                         verbose: Boolean = true)
  extends ThinController(0.001) with Actor {

  // here's our own, personal, neural network; nobody else's. it OURS ...
  val network = new SomHypeNeuralNetwork(
    Layer(numInputs, 100, Logistic) :+ Layer(numOutputs, SoftMax),
    objective = CrossEntropyError)

  // these are the initial weights, which we'll use to seed the germinal centre
  val weights: TSeq[Double] = weights2Sequence(network)

  val interface = new FixedPointSomHypeInterface(network, weights)

  def receive = {

    case ThisDataGC(iteration, data) =>
      if (verbose) log.info(s"${self.path} received DataGC")
      // got through the batch of data
      val errors: TSeq[Double] = interface.update(iteration, data)
      if (verbose) {
        val _errors=(0 until errors.length).map{ i =>
          errors(i)
        }.mkString(" ")
        log.info(s"${self.path} ${count} received ThisDataGC")
        log.info(s"${self.path} ${count} errors: ${_errors}")
      }
      // respond to the controller actor
      sender ! TheseErrorsGC(iteration, errors.toArray)


    case FinalWhistle =>
      log.info(s"${self.path} received FinalWhistle, shutting down")
      context.stop(self)


    case m =>
      log.info(s"${self.path} received unknown message from ${sender}, message ${m}")


  }

}

object FixedPointNetActor {

  // best practise is to put the props close to where the
  // actor itself is init'd
  def props(numInputs: Int, numOutputs: Int): Props = Props(new FixedPointNetActor(numInputs, numOutputs))

}
