package org.garagescience.deeplearning.nosgd.MNIST

import breeze.linalg._
import breeze.numerics._


// TODO: this is all pretty long & messy & unwieldy

class SomHypeNetwork(override val sizes: List[Int]) extends DeepLearningNetwork(sizes) {

  def learningAlgorithm(x: NetworkVector,
                        y: NetworkVector): (List[NetworkVector], List[NetworkMatrix]) = {

    val newB: Array[DenseVector[Double]] = biases.map(b => DenseVector.zeros[Double](b.length)).toArray
    val newW: Array[DenseMatrix[Double]] = weights.map(w => DenseMatrix.zeros[Double](w.rows, w.cols)).toArray

    var activation: NetworkVector = x

    var (zs, activations) = biases
      .zip(weights)
      .map({
        case (b, w) => {
          val z = w * activation + b
          activation = sigmoid(z)

          (z, activation)
        }
      })
      .unzip

    activations = x :: activations

    var delta: DenseVector[Double] = costDerivative(activations.last, y) :* sigmoidPrime(zs.last)

    newB(newB.length - 1) = delta
    newW(newW.length - 1) = delta * activations(activations.length - 2).t




    (newB.toList, newW.toList)
  }

}
