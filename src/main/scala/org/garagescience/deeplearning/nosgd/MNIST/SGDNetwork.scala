package org.garagescience.deeplearning.nosgd.MNIST

import breeze.linalg._
import breeze.numerics._

class SGDNetwork(override val sizes: List[Int]) extends DeepLearningNetwork(sizes) {


  def learningAlgorithm(x: NetworkVector, y: NetworkVector): (List[NetworkVector], List[NetworkMatrix]) = {
    val newB = biases.map(b => DenseVector.zeros[Double](b.length)).toArray
    val newW = weights.map(w => DenseMatrix.zeros[Double](w.rows, w.cols)).toArray

    var activation = x

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

    var delta = costDerivative(activations.last, y) :* sigmoidPrime(zs.last)

    newB(newB.length - 1) = delta
    newW(newW.length - 1) = delta * activations(activations.length - 2).t

    for (l <- 2 until numLayers) {
      val z = zs(zs.length - l)
      val sp = sigmoidPrime(z)
      delta = (weights(weights.length - l + 1).t * delta) :* sp

      newB(newB.length - l) = delta
      newW(newW.length - l) = delta * activations(activations.length - l - 1).t
    }

    (newB.toList, newW.toList)
  }


}


