package org.garagescience.deeplearning.nosgd.mlp.training

import org.garagescience.deeplearning.nosgd.mlp.{NeuralNetwork, BackPropNeuralNetwork, SomHypeNeuralNetwork}
import org.garagescience.deeplearning.nosgd.mlp.data.DataSet
import org.jblas.DoubleMatrix

import scala.collection.parallel.ParSeq

class Trainer(val numIterations: Int,
              val miniBatchSize: Int,
              val numParallel: Int,
              val learningRate: LearningFunction,
              val momentumMultiplier: Double,
              val gradientChecker: Option[GradientChecker],
              val evalIterations: Int) {

  // FIXME: N.B. the original version is below:
  // FIXME: ORIGINAL VERSION:
  /*
  def evalIteration(iteration: Int, network: NeuralNetwork, trainingSet: DataSet, batch: DataSet) {
    if ((iteration + 1) % evalIterations == 0) {
    // FIXME: NOTE: only triggers quite rarely, e.g. every 100, 1000 iterations!
      val loss = network.loss(trainingSet)
      println("Iteration:%5d, Loss: %.5f, Accuracy: %f".format(iteration + 1, loss, network.eval(trainingSet)))
      for (check <- gradientChecker) {
        check(network, batch)
      }
    }
  }
  // FIXME: so what's providing the error term & weights modification?!
   */

  def evalIteration(iteration: Int,
                    network: BackPropNeuralNetwork,
                    trainingSet: DataSet,
                    batch: DataSet) {
    // FIXME: interestingly, if you assess the loss here (like i do with my somhype version), it
    // FIXME: slows it down, *A* *LOT*
    // FIXME: so we have to ask what else they're doing in the code to train the network
    // FIXME: *WHATEVER IT IS*, it'll involve some kind of error term or adjustment to the
    // FIXME: weights - it HAS to - so we just find & use that!!!
    val loss = network.loss(trainingSet)
    println("Iteration:%5d, Loss: %.5f, Accuracy: %f".format(iteration + 1, loss, network.eval(trainingSet)))
    if ((iteration + 1) % evalIterations == 0) {
      for (check <- gradientChecker) {
        check(network, batch)
      }
    }
  }

  def train(network: BackPropNeuralNetwork, trainingSet: DataSet) {

    // FIXME: this clears the momentums term, also puts it in the right "shape" as the
    // FIXME: weights matrices
    val momentums: List[DoubleMatrix] = network.layers.map { _.weights.mul(0) }

    trainingSet.
      miniBatches(miniBatchSize).
      grouped(numParallel).
      take(numIterations).
      zipWithIndex.foreach { case (batches: Seq[DataSet], iteration: Int) =>
        // TODO: as noted, this only "does something" every n iterations
        evalIteration(iteration, network, trainingSet, batches(0))

        // FIXME: so, most of the adaptation must occur below.
        // FIXME: so, how does it work?!? & can we nick it?!

        val batchGradients: ParSeq[Seq[DoubleMatrix]] = batches.par.map { network.errorGradients(_) }
        val sumGradients = batchGradients.reduce {
          (sumGradients, gradients) =>
            sumGradients.zip(gradients).map {
              case (sumGradient, gradient) => sumGradient.addi(gradient)
            }
        }
        val gradients: Seq[DoubleMatrix] = sumGradients.map { _.muli(1D / numParallel) }


        updateWeights(network, iteration, gradients, momentums)
    }
  }

  def updateWeights(network: NeuralNetwork,
                    iteration: Int,
                    gradients: Seq[DoubleMatrix],
                    momentums: Seq[DoubleMatrix]) {
    network.layers.zip(gradients).zip(momentums).foreach {
      case ((layer, gradient), momentum) =>
        val delta: DoubleMatrix = momentum.muli(momentumMultiplier).subi(gradient)
        layer.weights.addi(delta.mul(learningRate(iteration)))
    }
  }
}


object Trainer {

  def apply(numIterations: Int,
            miniBatchSize: Int = 100,
            learningRate: LearningFunction = ConstantRate(0.5),
            momentumMultiplier: Double = 1.0,
            gradientChecker: Option[GradientChecker] = None,
            evalIterations: Int = 100,
            numParallel: Int = 1) =
    new Trainer(numIterations,
      miniBatchSize,
      if (numParallel < 1) 1 else numParallel,
      learningRate,
      momentumMultiplier,
      gradientChecker,
      evalIterations)

}