package org.garagescience.deeplearning.nosgd.actors

import akka.actor.{Actor, Props}
import org.garagescience.deeplearning.nosgd.mlp._
import org.garagescience.deeplearning.nosgd.mlp.NeuralNetwork._
import scala.language.postfixOps

// TODO: WE NEED A MORE SENSITIVE ERROR FUNCTION!


class SomHypeNetActor(numInputs: Int, numOutputs: Int, verbose: Boolean = false)
  extends ThinController(0.001) with Actor {

  // here's our own, personal, neural network; nobody else's. it OURS ...
  val network = new SomHypeNeuralNetwork(
    Layer(numInputs, 100, Logistic):+Layer(numOutputs, SoftMax),
    objective = CrossEntropyError)

  // these are the initial weights, which we'll use to seed the germinal centre
  val weights: Array[Double] = weights2Sequence(network)

  val interface = new SomHypeInterface(network, weights)

  // constrained GC here ...

  def receive = {

    case ThisDataGC(iteration, data) =>
      if (verbose) log.info(s"${self.path} received DataGC")
      // got through the batch of data
      val errors = interface.update(iteration, data)
      // respond to the controller actor
      sender ! TheseErrorsGC(iteration, errors)


      // FIXME: the methods below are STUB implementations. we may or
      // FIXME: need them ...


    // FIXME: STUB: this is a stub implementation, leave for now ...
    case GetErrorsGC =>
      if (verbose) log.info(s"${self.path} received ErrorsGC")
      // TODO: write this code!
      // val errors: Array[Double] = gc.getClonePoolFitness(error)
      val errors = Array(0.0)
      if (verbose) log.info(s"${self.path} errors: $errors")
      sender ! ErrorsGC(errors)


    // FIXME: STUB: this is a stub implementation, leave for now ...
    case GetMinimumGC =>
      log.info(s"${self.path} received GetMinimumGC")
      // get the fittest clone from the pool? yep, let's
      // do that ...
      val errors = Array(0.0)
      sender ! MinimumGC(errors)



    case FinalWhistle =>
      log.info(s"${self.path} received FinalWhistle, shutting down")
      context.stop(self)



    case m =>
      log.info(s"${self.path} received unknown message from ${sender}, message ${m}")
  }

}

object SomHypeNetActor {

  // best practise is to put the props close to where the
  // actor itself is init'd
  def props(numInputs: Int, numOutputs: Int): Props =
    Props(new SomHypeNetActor(numInputs, numOutputs))

}
