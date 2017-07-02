package org.garagescience.deeplearning.nosgd.XOR

import scala.collection.mutable.Buffer
import breeze.linalg.{DenseMatrix, DenseVector, max}
import math.abs

class SomHypeNeuralNetwork(_neuronCounts: Seq[Int],
                           _activationFunction: ActivationFunction,
                           val popSize: Int = 20,
                           override val useBias: Boolean = true)
  extends _NeuralNetwork(_neuronCounts = _neuronCounts, useBias = useBias) {

  // TODO: do we need this? ...
  def activationFunction = _activationFunction

  // TODO: move to the parent class?
  def getMaxDelta(): Double = delta.tail.map(_.map(abs(_)).max).max

  // TODO: maybe we should pull all this apart a bit more
  def trainImpl(input: Seq[Double], desiredResult: Seq[Double]) = {
    assert(input.length == V(0).length - 1)
    assert(desiredResult.length == V(M).length)

    classify(input) //sets V and h

    val eta: DenseVector[Double] = DenseVector(desiredResult : _*)

    // this be the error: N.B., M is layerCount - 1
    delta(M) := h(M).map(activationFunction.derivative) :* (eta - V(M))

    // we could also get the layer-by-layer backprop error, but let's go for
    // somatic hypermutation from here

    // so our targets are the weight matrix, which is def'd by w, where
    // w is a buffer of DenseMatrix[Double]. which we know how to Do
    // Stuff to ...



  }
}