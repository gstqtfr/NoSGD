package org.garagescience.deeplearning.nosgd.mlp

import scala.util.control.Exception
import scala.util.Random
import org.apache.spark.ml.linalg.{Vectors,Vector}
import org.apache.spark.ml.linalg.{Matrices,Matrix}

trait _Layer {

  import MathUtils._

  protected def size: Int
  protected def nextSize: Int=0
  protected def learningRate: Double=0.01
  protected val inputLayer: Boolean=false
  protected val outputLayer: Boolean=false
  protected var weights: Matrix
  protected var bias: Vector

  /**
    * Feeds this layer with the inputs from the previous layer and returns
    * the output
    *
    * @param features  Outputs from the previous layer
    * @param prevLayer The previous layer in the network
    * @return          Outputs of this layer
    */
  def feed(features: Vector, prevLayer: Layer): Vector = {
    val (prevW, prevB) = prevLayer.get

    inputs = prevW.multiply(features) + prevB
    outputs = activation(inputs)
    outputs
  }

  protected var inputs = Vectors.zeros(size)
  protected var outputs = Vectors.zeros(size)
  protected var errors = Vectors.zeros(size)

  val activation: (Vector) => Vector = if (outputLayer) Neuron.identity(_) else Neuron.sigmoid(_)
  val derivative: (Vector) => Vector = if (outputLayer) Neuron.identityPrime(_) else Neuron.sigmoidPrime(_)

  override def toString = s"=> ${outputs.size} neuron(s)"

  /**
    * Returns the weights and bias contained in this layer
    */
  def get = (weights, bias)

  def set(w: Matrix, b: Vector) = {
    weights = w
    bias = b
  }

  /**
    * Sets the inputs and outputs of this layer.
    * This function is used only to set the values of the input layer
    */
  def setInput(input: Vector) = {
    inputs = input
    outputs = inputs
  }

  def update(target: Vector): Vector =
    derivative(outputs).elementProd(outputs - target)


}