package org.garagescience.deeplearning.nosgd.XOR

import scala.math.exp

/**
  * the [[https://en.wikipedia.org/wiki/Sigmoid_function sigmoid function]] used as the activation function for
  * the neural networks.
  *
  * Note, that it's values are from 0.0 to 1.0, and the neural network's inputs (both the vectors of data being classified
  * and the data used to train the nn has to be also comprized of 0s and 1s, or at least in the range.
  */
// protected
case class SigmoidFunction(val beta: Double) extends ActivationFunction {

  def apply(h: Double): Double = {
    1.0 / (1.0 + exp(-2.0 *  beta * h))
  }

  def derivative(h: Double): Double = {
    val fx = apply(h)
    2.0 * beta * fx * (1.0 - fx)
  }

  def bounds = (0, 1)
}
