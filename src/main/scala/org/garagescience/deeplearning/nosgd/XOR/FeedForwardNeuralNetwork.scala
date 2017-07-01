package org.garagescience.deeplearning.nosgd.XOR

import scala.collection.mutable.Buffer
import breeze.linalg.{DenseMatrix, DenseVector, max}
import math.abs

/**
  * layerCounts describes the number of neurons in each layer
  * layerCounts(0) is the number of input neurons (NOT including the weighing unit - 0)
  * layerCounts(layerCounts.length - 1) is the number of output neurons
  *
  * beta - steepness of the sigmoid function
  * @param gamma - scaling parameter of corrections - the "learning rate" parameter
  */
class FeedForwardNeuralNetwork( _neuronCounts: Seq[Int],
                                _activationFunction: ActivationFunction,
                                val gamma: Double,
                                override val useBias: Boolean = true)
  extends _NeuralNetwork(_neuronCounts = _neuronCounts, useBias = useBias) {

  // TODO: do we need this? ...
  def activationFunction = _activationFunction

  // TODO: move to the parent class?
  def getMaxDelta(): Double = delta.tail.map(_.map(abs(_)).max).max

  protected def trainImpl(input: Seq[Double], desiredResult: Seq[Double]): Unit = {
    assert(input.length == V(0).length - 1)
    assert(desiredResult.length == V(M).length)

    classify(input) //sets V and h

    val eta: DenseVector[Double] = DenseVector(desiredResult : _*)

    //backpropagation - last layer
    delta(M) := h(M).map(activationFunction.derivative) :* (eta - V(M))

    //backpropagation - rest of the layers
    for(m <- Range(M-1, 1, -1).inclusive) {
      delta(m) := h(m).map(activationFunction.derivative) :* (w(m).t * delta(m+1))
      delta(m)(0) = 0.0
    }

    //adjusting the weights
    for(m <- Range(1, M).inclusive) {
      val weightAdjustments = (delta(m) * V(m-1).t :* gamma)
      w(m-1) := w(m-1) + weightAdjustments
    }
  }
}