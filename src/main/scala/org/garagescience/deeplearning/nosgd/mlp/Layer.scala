package org.garagescience.deeplearning.nosgd.mlp

import scala.util.Random
import org.apache.spark.ml.linalg.{Matrices, Matrix, Vector, Vectors}

/**
  * Class representing a layer of neurons in a neural network
  *
  * @param size          Number of neuron in this layer
  * @param learningRate  The learning rate to use for training
  * @param inputLayer    Boolean that says if this layer is the input layer
  * @param outputLayer   Boolean that says if this layer if the output layer
  */
class Layer(override val size: Int,
            override val nextSize: Int=0,
            override val learningRate: Double=0.01,
            override val inputLayer: Boolean=false,
            override val outputLayer: Boolean=false) extends _Layer {

  // TODO: JKK: do we need the overrides above?

  // TODO: okay. to get the error, we just need to compare the output of
  // TODO: network with the target for this pattern. er, that's it.

  // TODO: actually, this is more appropriate at the network layer, so
  // TODO: let's do it there ...


  // TODO: refactor?
  override var (weights: Matrix, bias: Vector) = outputLayer match {
    case true  => (Matrices.zeros(1, 1), Vectors.zeros(1))
    case false =>
      (Matrices.dense(nextSize, size, (for (i <- 0 to (nextSize * size - 1))
        yield Random.nextDouble).toArray),
        Vectors.dense((for (i <- 0 to (nextSize - 1))
          yield Random.nextDouble).toArray))
  }

}