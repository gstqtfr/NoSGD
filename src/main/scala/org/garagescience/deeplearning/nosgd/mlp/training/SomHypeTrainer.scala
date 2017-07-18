package org.garagescience.deeplearning.nosgd.mlp.training

import org.garagescience.deeplearning.nosgd.bca.ConstrainedSequenceHypermutation
import org.garagescience.deeplearning.nosgd.mlp.{Layer, NeuralNetwork, SomHypeNeuralNetwork}
import org.garagescience.deeplearning.nosgd.mlp.data.DataSet
import org.jblas.DoubleMatrix

import scala.collection.parallel.ParSeq

// TODO: when this is sorted, we need to get the actors up & running quickly
// TODO: this is pretty slow w'out parallelisation

// TODO: need to add a param. to say of we want to minimise or maximise ...
// TODO: set embiggen to true or false ...


class SomHypeTrainer(val numIterations: Int,
                     val miniBatchSize: Int,
                     val numParallel: Int,
                     val learningRate: LearningFunction,
                     val momentumMultiplier: Double,
                     val gradientChecker: Option[GradientChecker],
                     val evalIterations: Int) {

  def evalIteration(iteration: Int,
                    network: NeuralNetwork,
                    hypermutate: ConstrainedSequenceHypermutation,
                    trainingSet: DataSet,
                    batch: DataSet) = {

    //println(s"SomHypeTrainer: in evalIteration, iteration $iteration")

    if ((iteration + 1) % evalIterations == 0) {
      val loss: Double = network.loss(trainingSet)
      //println(s"SomHypeTrainer: in evalIteration, loss $loss")
      val errorTerm: Double = network.eval(trainingSet)
      //println(s"SomHypeTrainer: in evalIteration, error term $errorTerm")

      //if ((iteration + 1) % evalIterations == 0) {
      println("Iteration:%5d, Loss: %.5f, Accuracy: %f".format(iteration + 1, loss, errorTerm))
      //}

      //println(s"SomHypeTrainer: in evalIteration, calling update weights")
      /*

    updateWeights(
      network,
      hypermutate,
      trainingSet,
      iteration)
*/

      //errorTerm
    }
  }

  // TODO: move this somewhere we don't have repeat the bloody declaration!!!
  def weights2Sequence(network: NeuralNetwork) = network.layers.
    map(layer => layer.weights).
    map(weights => weights.toArray).flatten.toArray


  def applySequence2Weights(clone: Array[Double],
                            layers: Array[Layer]) = {

    // println("SomHypeTrainer: in applySequence2Weights")

    val _iterator = clone.iterator

    layers.
      map(layer => layer.weights).
      map(matrix => matrix.data).
      map(data => {
        (0 until data.length).map { index =>
          data(index) = _iterator.next
        }
      })
  }


  // TODO: so this is going to get all forensic on the train method, see if we can
  // TODO: get it picked apart & understand it more fully, so we can have
  // TODO: a good idea of how it works & apply it to SomHype network ...


  def train(network: SomHypeNeuralNetwork,
            hypermutate: ConstrainedSequenceHypermutation,
            trainingSet: DataSet,
            iterations: Int = 500) = {

    println("SomHypeTrainer: in train")

    // TODO: look at numIterations here ...
    // TODO: also, if the take function is doing what i think it is, we need
    // TODO: to GET A RANDOM SAMPLE from the data set ...
    trainingSet.
      miniBatches(miniBatchSize).
      grouped(numParallel).
      take(iterations).
      zipWithIndex.foreach {
      case (batches, iteration) =>
        evalIteration(iteration, network, hypermutate, trainingSet, batches(0))

        /*
        val batchGradients: ParSeq[Seq[DoubleMatrix]] = batches.par.map { network.errorGradients(_) }
        val sumGradients = batchGradients.reduce {
          (sumGradients, gradients) =>
            sumGradients.zip(gradients).map {
              case (sumGradient, gradient) => sumGradient.addi(gradient)
            }
        }
        val gradients: Seq[DoubleMatrix] = sumGradients.map { _.muli(1D / numParallel) }
        */

        // TODO: let's print this out:

        /*
        println("SomHypeNetwork: train: gradients: ")
        gradients.foreach { grad =>
          println(s"$grad")
        }
        */

      // TODO: updateWeights takes:
        /*
        updateWeights(network: NeuralNetwork,
                      hypermutate: ConstrainedSequenceHypermutation,
                      trainingSet: DataSet,
                      iteration: Int)
         */
        // TODO: so let's wire it in ...

        // TODO: do we need to minimise the gradients? is that what we should put in here?
        // TODO: let's have a look at matey-boy's, see what he does ...
        updateWeights(network, hypermutate, trainingSet, iteration)

    }

  }


  // TODO: here's the update weights method from Trainer:

  /*

  // TODO: so, we'll ignore the momentum term as a complication
  // TODO: we don't need just now ...

  // TODO: val delta: DoubleMatrix = momentum.muli(momentumMultiplier).subi(gradient)
  // TODO: subi(matrix m): Subtract a matrix (in place).
  // TODO: so it takes the gradient away from the momentum matrix
  // TODO: then we have the "momentum" double, which multiplies it in place
  // TODO: then we have:
  // TODO: layer.weights.addi(delta.mul(learningRate(iteration)))
  // TODO: which, ignoring the persiflage, adds this stuff to the weights
  // TODO: so that's delta

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



  */





  // apply the clone to the network weights, evaluate, then return the
  // errorTerms
  def cloneTrial(clone: Array[Double],
                 network: NeuralNetwork,
                 trainingSet: DataSet): Double = {
    //println("SomHypeTrainer: in cloneTrial")
    applySequence2Weights(clone, network.layers.toArray)
    network.eval(trainingSet)
  }

  def updateWeights(network: NeuralNetwork,
                    hypermutate: ConstrainedSequenceHypermutation,
                    trainingSet: DataSet,
                    iteration: Int) = {

    // println("SomHypeTrainer: in updateWeights")
    // this gets the current error response of the network
    val currentFitness = network.eval(trainingSet)

    // produce the next generation of hypermutated clones
    val newClonalPool: Array[Array[Double]] = hypermutate.germinate

    // update the clonal pool with hypermutated new clones
    // try the new weights, derived from the clone, & get the error terms for
    // each clone
    val newFitness: Array[Double] = newClonalPool.map(clone => cloneTrial(clone, network, trainingSet))

    // get the fittest clone
    val newWeights: Array[Double] = newClonalPool(getFittest(newFitness))

    // now we need to apply the best weights to the network weights
    applySequence2Weights(newWeights, network.layers.toArray)

  }


  def getFittest(errors: Array[Double]): Int = {
    //println("SomHypeTrainer: in getFittest")
    import scala.collection.immutable.ListMap
    val m1 = (errors.indices zip errors).toMap
    // TODO: we want the highest score here
    //ListMap(m1.toSeq.sortBy(_._2): _*).head._1
    ListMap(m1.toSeq.sortWith(_._2 > _._2): _*).head._1
  }


}


object SomHypeTrainer {

  def apply(numIterations: Int,
            miniBatchSize: Int = 100,
            learningRate: LearningFunction = ConstantRate(0.5),
            momentumMultiplier: Double = 1.0,
            gradientChecker: Option[GradientChecker] = None,
            evalIterations: Int = 10,
            numParallel: Int = 1) =
    new SomHypeTrainer(numIterations,
      miniBatchSize,
      if (numParallel < 1) 1 else numParallel,
      learningRate,
      momentumMultiplier,
      gradientChecker,
      evalIterations)

}