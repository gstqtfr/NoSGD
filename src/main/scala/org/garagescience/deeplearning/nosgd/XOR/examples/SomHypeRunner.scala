package org.garagescience.deeplearning.nosgd.XOR.examples

import Helpers._
import akka.actor.{ActorRef, ActorSystem}
import org.garagescience.deeplearning.nosgd.linalg._
import org.garagescience.deeplearning.nosgd.XOR._
import org.garagescience.deeplearning.nosgd.akka.{GCController, GerminalCentreActor}
import scala.collection.immutable.Seq
import scala.util.Random

object SomHypeRunner {

  private def creatActor(sys: ActorSystem,
                         _init: () =>Matrix[Double],
                         _error:Matrix[Double] => Double): ActorRef = {
    sys.actorOf(GerminalCentreActor.props(_init(), _error))
  }

  private final val popSz = 10

  //we will be using the boolean interface, hence the boolean values
  private val possibleInputs = List(
    List(false, false),
    List(false, true),
    List(true, false),
    List(true, true)
  )

  /*
   * List of neuron counts for each layer:
   * two in the input layer, two in the hidden layer, one in the output layer
   *
   * the bias neurons are hidden "under the hood"
   */
  val neuronsInLayers = List(2, 3, 1)

  // here's our activation function
  val sigmoid = new SigmoidFunction(1.9)
  val epsilon = 0.001


  // TODO: REFACTOR: the original code had a few of these, large lumps of code
  // TODO: that could be refactored quite easily; so let's think about how to
  // TODO: break these guys up ...

  // TODO: BIAS TERM: we ignore this for now, BIAS TERM stays out of the germinal
  // TODO: centre

  /**
    * A classic, small example of a neural network is a network is a network calculating the XOR
    * function. A network in the simplest form is not able to do it - we need a network with
    * at least two neurons in the hidden layer. Let's see how it works.
    *
    * Note: this problem, despite its apparent simplicity is hard to solve ...
    */
  def xorExample(): Unit = {

    //creating the somatic-hypermutation-based neural network

    val xornn: _NeuralNetwork = new SomHypeNeuralNetwork(neuronsInLayers, sigmoid, popSz)

    val system = ActorSystem("TestGCmultipleActorSytem")


/*

    val gcl: List[ActorRef] = {for {i <- 0 until popSz} yield creatActor(system,
      // TODO: so, do we create an initial matrix function for each of these guys? or do we
      // TODO: just half-inch the weights from the nn?
      // TODO: PROBLEM: w is a Buffer[DenseMatrix[Double]]
      // TODO: also, a breeze.linalg.DenseMatrix, not ml.linalg.Matrix!!!
      xornn.w,
      // TODO: need to provide the error function for these guys ...
      error)}.toList

    // TODO: we won't have a clear target in the way we did with the previous problem on matrices, since
    // TODO: we simply *don't* *know* what the right matrix is. so we need to modify the code here ...
    val gcc: ActorRef = system.
      actorOf(GCController.props(target, gcl, epsilon),
        name = "gcl")

    /**
      * now let's see how many times we have to teach the network all the examples in order for it
      * to calculate the xor correctly
      */

    var it: Int = 0
    var correctAnswersCount: Int = 0

    val r = new Random()
    while (correctAnswersCount != 4 && it <= 10000) {
      //teaching the nn
      for (ex <- possibleInputs) {
        xornn.trainBool(ex, Seq(ex.head ^ ex.last))
      }

      //checking if it gives the correct answers already
      // TODO: this is horrid - change it
      val correctAnswers = for (
        pi <- possibleInputs;
        answer = xornn.classifyBool(pi).head;
        correctAnswer = pi.head ^ pi.last
      ) yield answer == correctAnswer

      correctAnswersCount = correctAnswers.count(b => b)
      System.out.println("%d iteration: %d correct answers" format(it, correctAnswersCount))
      it += 1;
    }

    if (it > 10000) {
      System.out.println("the system has reached a local optimum without hitting the correct solution - this can also happen :)")
    }
*/
  }

  def main(args: Array[String]): Unit = {
    time {
      //choose the example you want to run
      //irisExample()
      xorExample()
      //parityExample()
    }
  }

}