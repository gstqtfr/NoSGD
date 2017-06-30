package org.garagescience.deeplearning.nosgd.MNIST

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.generic.{MappingUFunc, UFunc}
import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.Rand
import scala.util.Random


// do we need this? it's nice & everything, but is it necessary?
trait NetworkTypes {
  type NetworkVector = DenseVector[Double]
  type NetworkMatrix = DenseMatrix[Double]
  type TrainingSet = List[(NetworkVector, NetworkVector)]
}

object sigmoidPrime extends UFunc with MappingUFunc {

  implicit object sigmoidImplDouble extends Impl[Double, Double] {
    def apply(x: Double) = sigmoid(x) :* (1d - sigmoid(x))
  }

}

abstract class DeepLearningNetwork(protected val sizes: List[Int]) extends NetworkTypes {

  protected def learningAlgorithm(x: NetworkVector, y: NetworkVector):
  (List[NetworkVector], List[NetworkMatrix])

  protected val numLayers: Int = this.sizes.length

  protected var biases: List[NetworkVector] = this.sizes
    .drop(1)
    .map(n => DenseVector.rand(n, Rand.gaussian))

  var weights: List[NetworkMatrix] = this.sizes
    .drop(1)
    .zip(this.sizes.dropRight(1))
    .map({ case (r, c) => DenseMatrix.rand(r, c, Rand.gaussian) })

  def feedForward(activation: NetworkVector): NetworkVector = {
    var result = activation
    this.biases
      .zip(this.weights)
      .foreach({ case (b, w) => result = sigmoid(w * result + b) })

    result
  }

  def evaluate(testData: TrainingSet): Int = {
    testData
      .map({ case (x, y) => (argmax(feedForward(x)), argmax(y)) })
      .count { case (x, y) => x == y }
  }

  def costDerivative(outputActivations: NetworkVector, y: NetworkVector): NetworkVector = {
    outputActivations - y
  }

  def minimiseError(trainingData: TrainingSet,
                    epochs: Int,
                    miniBatchSize: Int,
                    eta: Double,
                    testData: Option[TrainingSet]): Unit = {

    var shuffledTrainingData = trainingData

    for (x <- 0 until epochs) {
      shuffledTrainingData = Random.shuffle(shuffledTrainingData)

      val miniBatches = shuffledTrainingData.grouped(miniBatchSize).toList

      miniBatches.foreach(b => updateMiniBatch(b, eta))

      testData match {
        case Some(d) => println(s"Epoch $x: ${evaluate(d)} / ${d.length}")
        case None => println(s"Epoch $x complete")
      }
    }
  }

  def updateMiniBatch(batch: TrainingSet, eta: Double): Unit = {
    var newB = biases.map(b => DenseVector.zeros[Double](b.length))
    var newW = weights.map(w => DenseMatrix.zeros[Double](w.rows, w.cols))

    batch.foreach({
      case (x, y) => {
        val (deltaB, deltaW) = learningAlgorithm(x, y)

        newB = newB.zip(deltaB).map({ case (nb, db) => nb + db })
        newW = newW.zip(deltaW).map({ case (nw, dw) => nw + dw })

      }
    })

    biases = biases.zip(newB).map({ case (b, nb) => b - (eta / batch.length) * nb })
    weights = weights.zip(newW).map({ case (w, nw) => w - (eta / batch.length) * nw })
  }

}