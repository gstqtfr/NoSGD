package org.garagescience.deeplearning.nosgd.XOR

import breeze.linalg.{DenseMatrix, DenseVector}

import scala.collection.mutable.Buffer

/**
  * a common trait for all neural networks. All inheriting classes need to implement
  * [[org.garagescience.deeplearning.nosgd.XOR!teachImpl]]
  * and `classifyImpl` methods and expose their activation function via `activationFunction`
  */
trait NeuralNetwork {

  private val oob: String =
    "The `teach` and `classify` vectors' arguments' values should be within bounds of activationFunction"

  def activationFunction: ActivationFunction

  /**
    * the classification function to be implemented by subclasses
    */
  protected def classifyImpl(input: Seq[Double]): Seq[Double]

  /**
    * wraps the `classifyImpl` method with bounds check.
    *
    * @param input a sequence of Doubles - the vector being classified. Note: This method will make sure the values are
    *              within the bounds defined by the `activationFunction`
    *
    *              returns the sequence - vector of Doubles the input was classified as. The result is not rounded and
    *              to get the
    *              class the example has been classified to you have to adequately round the values, eg. using
    *              [[org.garagescience.deeplearning.nosgd.XOR.ActivationFunction]]'s methods */

  def classify(input: Seq[Double]): Seq[Double] = {
    require(input.forall(activationFunction.withinBounds(_)), oob)

    classifyImpl(input)
  }

  /**
    * returns maximum correction made in the last training of the neural network. This can be used for termination
    * criteria for training the network.
    */
  def getMaxDelta(): Double


  /**
    * wraps the `teachImpl` method with bounds check.
    *
    * @param input a sequence of Doubles - the vector being classified. Note: This method will make sure the values are
    * within the bounds defined by the `activationFunction`
    *
    * @param desiredResult the sequence - vector of Doubles the input should be classified as. This parameter will also
    * be verfied for being within the bounds defined by `activationFunction`
    */
  def train(input: Seq[Double], desiredResult: Seq[Double]): Unit = {
    assert(input.forall(activationFunction.withinBounds(_)), oob)
    assert(desiredResult.forall(activationFunction.withinBounds(_)), oob)

    trainImpl(input, desiredResult)
  }

  /**
    * the teaching function to be implemented by subclasses. If in the course of teaching you need to invoke the
    * classification method, use `teach` not `teachImpl` for added arguments checks
    */
  protected def trainImpl(input: Seq[Double], desiredResult: Seq[Double]): Unit

  /*
   * Other interfaces in case it's easier to deal with Ints or Bools
   */
  /**
    * Int interface for `classify`
    */
  def classifyInt(input: Seq[Int]): Seq[Int] = {
    classify(input.map(_.toDouble)).map(activationFunction.customRound(_))
  }

  /**
    * Int interface for `teach`
    */
  def trainInt(input: Seq[Int], desiredResult: Seq[Int]): Unit = {
    val i2 = input.map(_.toDouble)
    val d2 = desiredResult.map(_.toDouble)
    train(i2, d2)
  }

  /**
    * Bool interface for `classify`
    */
  def classifyBool(input: Seq[Boolean]): Seq[Boolean] = {
    classify(input.map(activationFunction.fromBoolean(_).doubleValue)).map(activationFunction.toBoolean(_))
  }

  /**
    * Bool interface for `teach`
    */
  def trainBool(input: Seq[Boolean], desiredResult: Seq[Boolean]): Unit = {
    val i2 = input.map(activationFunction.fromBoolean(_).doubleValue)
    val d2 = desiredResult.map(activationFunction.fromBoolean(_).doubleValue)
    train(i2, d2)
  }
}

abstract class _NeuralNetwork(_neuronCounts: Seq[Int],
                              val useBias: Boolean = true) extends NeuralNetwork {

  protected val BIAS_VALUE: Double = if(useBias) 1.0 else 0.0
  protected val MAX_ABSOLUTE_WEIGHT_VALUE: Double = 1.0

  protected val layerCount = _neuronCounts.size
  protected val M = layerCount - 1

  /**
    * the neuron counts given in the constructor adjusted to account for the bias neurons
    */
  protected val neuronCounts: Seq[Int] = _neuronCounts.map( _ + 1).updated(M, _neuronCounts.last)

  
  /**
    * neuron state vectors
    */
  protected val V: Buffer[DenseVector[Double]] =
  (neuronCounts map { layerCount => DenseVector.ones[Double](layerCount)}).toBuffer

  /**
    * activation vectors
    */
  protected val h: Buffer[DenseVector[Double]] =
  (neuronCounts map { layerCount => DenseVector.ones[Double](layerCount)}).toBuffer


  /**
    * errors
    */
  protected val delta: Buffer[DenseVector[Double]] =
  (neuronCounts map { layerCount => DenseVector.ones[Double](layerCount)}).toBuffer

  delta(0) *= 0.0

  protected val _w: Iterator[DenseMatrix[Double]] =
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
  protected val w: Buffer[DenseMatrix[Double]] = _w.toBuffer

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
}