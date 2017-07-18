package org.garagescience.deeplearning.nosgd.akka

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import breeze.linalg.DenseVector
import org.garagescience.deeplearning.nosgd.XOR.{FeedForwardNeuralNetwork, NeuralNetwork, SigmoidFunction}

import scala.collection.immutable.{IndexedSeq, Seq => TSeq}
import org.garagescience.deeplearning.nosgd.linalg._
import org.garagescience.deeplearning.nosgd.{AckUpdateGC, _}

import scala.collection.mutable.Buffer
import scala.collection.immutable.{Seq => TSeq}
import scala.language.postfixOps
import scala.reflect.ClassTag

object XORMultipleController {

  // TODO: move to an implicits class?
  // TODO: *really* not sure about a Buffer being used here!?
  def fromDenseVectorsToLinalgMatrix[T: ClassTag](h: Buffer[DenseVector[T]]): Matrix[T] = {
    val vxs: Array[Vector[T]] = h.toSeq.map(dv => Vector[T](dv.toArray: _*)).toArray
    val builder = MatrixBuilder[T]()
    for (row <- 0 until vxs.length)
      builder += vxs(row)

    builder.result()
  }




  // the init. we pass here we may not need; could just be a copy of the
  // original nn weights matrix ... think about this one ...
  private def creatActor(sys: ActorSystem,
                         _init: () => Matrix[Double],
                         _error: Matrix[Double] => Double): ActorRef = {
    sys.actorOf(GerminalCentreMatrixActor.props(_init(), _error))
  }

  // TODO: review these Bad Boys ...
  private final val popSz = 10
  private final val epsilon = 0.001



  def main(args: Array[String]): Unit = {


    val system = ActorSystem("XORMultipleController")


    //we will be using the boolean interface, hence the boolean values
    val possibleInputs = List(List(0.0, 0.0),
      List(0.0, 1.0),
      List(1.0, 0.0),
      List(1.0, 1.0))

    // TODO: list of list here?
    val possibleOutputs = List(0.0, 1.0, 1.0, 0.0)

    /*
     * List of neuron counts for each layer:
     * two in the input layer, two in the hidden layer, one in the output layer
     *
     * the bias neurons are hidden "under the hood"
     */
    val neuronsInLayers = List(2, 3, 1)

    val sigmoid = new SigmoidFunction(1.9)

    val gamma = 0.8
    //creating the Neural network
    val xornn: NeuralNetwork = new FeedForwardNeuralNetwork(neuronsInLayers, sigmoid, gamma)

    def init  = xornn.getWeights

    // TODO: we need to get the weights from the neural network, so we need
    // TODO: to get them here ...

    val gcl: List[ActorRef] =
      {for {i <- 0 until popSz} yield creatActor(system, init, error)}.toList



  }




}