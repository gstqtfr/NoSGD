package org.garagescience.deeplearning.nosgd.matrixlevel

import org.garagescience.deeplearning.nosgd.mlp._
import org.garagescience.deeplearning.nosgd.mlp.data.DataSet
import org.jblas.DoubleMatrix

class MatrixSomHypeInterface(numInputs: Int,
                             numOutputs: Int,
                             val alpha: Double,
                             verbose: Boolean = false) {

  import NeuralNetwork._

  // we create our own neural network & our own hypermutation class
  // this gives clearer demarcation of responsibiities, makes the class
  // more self-contained ...

  // here's our own, personal, neural network; nobody else's. it OURS ...
  // TODO: what about 100 here? do we keep this, param, what!?!
  val network: SomHypeNeuralNetwork = new SomHypeNeuralNetwork(
    Layer(numInputs, 100, Logistic):+Layer(numOutputs, SoftMax),
    objective = CrossEntropyError)

  val weights: List[DoubleMatrix] = weights2MatrixSequence(network)

}