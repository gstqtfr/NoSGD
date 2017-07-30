package org.garagescience.deeplearning.nosgd.mlp

import org.garagescience.deeplearning.nosgd.mlp.data.DataSet
import org.jblas.DoubleMatrix

// TODO: make this generic? e.g.. higher kind on DoubleMatrix?
// TODO: protected?

trait NeuralNetwork {

  val layers: List[Layer]
  val objective: ObjectiveFunction

  def copy(layers: List[Layer]): NeuralNetwork

  def forwardProp(inputs: DoubleMatrix): List[LayerState]  =
  layers.scanLeft(LayerState(None, inputs)) {
    case (LayerState(_, x), layer) => layer(x)
  }.tail

  def loss(data: DataSet): Double

  def eval(data: DataSet): Double = {
    val outputs = forwardProp(data.inputs)
    val scores = outputs.last.activationOutput.columnArgmaxs.zip(data.targets.columnArgmaxs).map {
      case (p, t) =>
        if (p == t) 1 else 0
    }
    scores.sum.toDouble / scores.length
  }

}

object NeuralNetwork {

  def weights2Sequence(network: NeuralNetwork): List[Double] = network.
    layers.
    map(layer => layer.weights).
    map(weights => weights.toArray).flatten

  def weights2MatrixSequence(network: NeuralNetwork): List[DoubleMatrix] = network.
    layers.
    map(layer => layer.weights)

}