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


  def trainImpl(input: Seq[Double], desiredResult: Seq[Double]) = ???
}