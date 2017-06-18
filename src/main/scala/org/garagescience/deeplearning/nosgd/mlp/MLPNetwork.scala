package org.garagescience.deeplearning.nosgd.mlp

import org.apache.spark.ml.linalg.{Vectors,Vector}
import org.apache.spark.ml.linalg.{Matrices,Matrix}

trait MLPNetwork {

  // TODO: to get the doubles out of the Vector, all you
  // TODO: is call Vector.toArray:
  // TODO: def toArray: Array[Double]
  // TODO: then we can do 'orrible things to it
  // TODO: then we can slap 'em back in the network ...

  import MathUtils._

  def layers: Array[Layer]

  def getWeights: Seq[Matrix] = layers.map(l => l.weights).toSeq

  def getBiases: Seq[Vector] = layers.map(l => l.bias).toSeq

  // basic distance measure here, i think ...
  def getError(feature: Vector, output: Vector): Double = {
    val v: Double = (feature - output).sum.abs
    Math.pow(Math.sqrt(v), 2.0)
  }


  /**
    * Trains the neural network with the provided inputs and outputs
    *
    * @param features   Array containing the inputs of the network. The number of
    *                   values in each Vector must correspond to the number of
    *                   input neurons
    * @param targets    Array containing the outputs of the network. The number of
    *                   values in each Vector must correspond to the number of
    *                   output neurons
    * @param iterations The number of iterations to perform to train the network
    */
  def fit(features: Array[Vector],
          targets: Array[Vector],
          f: (Vector, Vector) => Vector,
          iterations: Int=1000) = {
    0 to iterations foreach { i =>
      features.zip(targets) foreach {
        case (_f, _t) => f(_f, _t)
      }
    }
  }


  /**
    * Makes a prediction given an input Vector
    *
    * @param features  Vector containing the input values. It must contain the
    *                  same number of values that the number of input neurons
    * @return          A Vector containing the prediction
    */
  def predict(features: Vector): Vector =
    feedForward(features)



  /**
    * Propagates an input in all the layers and returns the output
    *
    * @param features  Vector containing the input values
    * @return          A vector containing the prediction
    */
  def feedForward(features: Vector): Vector = {
    var inputs = features
    var prev = layers.head

    prev setInput inputs
    layers.slice(1, layers.length) foreach { layer =>
      inputs = layer.feed(inputs, prev)
      prev = layer
    }
    inputs
  }


}

