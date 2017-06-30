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
                                val useBias: Boolean = true) extends NeuralNetwork {

  // FIXME: something to consider here: do we *need* to adapt the bias terms?
  // FIXME: or can we leave them? in which case we just adapt the weights!
  // FIXME: which'll leave us just matrices & simplify the code
  private val BIAS_VALUE: Double = if(useBias) 1.0 else 0.0
  private val MAX_ABSOLUTE_WEIGHT_VALUE: Double = 1.0

  def activationFunction: ActivationFunction = _activationFunction

  val layerCount = _neuronCounts.size
  private val M = layerCount - 1

  /**
    * the neuron counts given in the constructor adjusted to account for the bias neurons
    */
  val neuronCounts: Seq[Int] = _neuronCounts.map( _ + 1).updated(M, _neuronCounts.last)

  /**
    * neuron state vectors
    */
  val V: Buffer[DenseVector[Double]] =
    (neuronCounts map { layerCount => DenseVector.ones[Double](layerCount)}).toBuffer

  /**
    * activation vectors
    */
  val h: Buffer[DenseVector[Double]] =
    (neuronCounts map { layerCount => DenseVector.ones[Double](layerCount)}).toBuffer

  /**
    * errors
    */
  val delta: Buffer[DenseVector[Double]] =
    (neuronCounts map { layerCount => DenseVector.ones[Double](layerCount)}).toBuffer
  delta(0) *= 0.0

  private val _w: Iterator[DenseMatrix[Double]] =
    for(ns <- neuronCounts.sliding(2)) yield {
      assert(ns.length == 2)
      val prevSize = ns.head
      val nextSize = ns.last
      //random weights at the beginning
      DenseMatrix.rand(nextSize, prevSize).map(x => 2.0 * MAX_ABSOLUTE_WEIGHT_VALUE * x - 1.0)
    }

  /**
    * (layerCount - 1) matrices of connection weights between layers
    *
    * the sum (of influences) for i+1 can be calculated by w(i) * V(i) [matrix multiplication]
    */
  val w: Buffer[DenseMatrix[Double]] = _w.toBuffer

  def getMaxDelta(): Double = delta.tail.map(_.map(abs(_)).max).max

  def classifyImpl(input: Seq[Double]): Seq[Double] = {
    assert(input.length == V(0).length - 1)

    V(0) := DenseVector((BIAS_VALUE +: input) : _*) //setting the input layer values

    //forward propagation
    for(i <- Range(0, M)) {
      h(i+1) := w(i) * V(i)

      V(i+1) := h(i+1).map(activationFunction)
      if(i+1 != M) {
        h(i+1)(0) = Double.MaxValue //this is actually redundant but might be better for transparency reasons
        V(i+1)(0) = BIAS_VALUE
      }
    }

    V(M).toArray
  }

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