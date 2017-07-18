package org.garagescience.deeplearning.nosgd.training

import org.garagescience.deeplearning.nosgd.bca.ConstrainedSequenceHypermutation
import org.garagescience.deeplearning.nosgd.mlp.{Layer, NeuralNetwork, SomHypeNeuralNetwork}
import org.garagescience.deeplearning.nosgd.mlp.data.DataSet
import org.garagescience.deeplearning.nosgd.mlp.training.GradientChecker
import org.jblas.DoubleMatrix
import scala.collection.parallel.ParSeq

class SimpleSomHypeTrainer(val numIterations: Int,
                           val miniBatchSize: Int,
                           val numParallel: Int,
                           val momentumMultiplier: Double,
                           val gradientChecker: Option[GradientChecker],
                           val evalIterations: Int) {

  def applySequence2Weights(clone: Array[Double],
                            layers: Array[Layer]) = {

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

  def updateWeights(network: NeuralNetwork,
                    hypermutate: ConstrainedSequenceHypermutation,
                    trainingSet: DataSet,
                    iteration: Int): Array[Double] = {

    // println("SomHypeTrainer: in updateWeights")
    // this gets the current error response of the network

    // TODO: network.eval! fine ...
    val currentFitness = network.eval(trainingSet)
    println(s"$iteration $currentFitness")

    // produce the next generation of hypermutated clones
    val newClonalPool: Array[Array[Double]] = hypermutate.germinate

    // update the clonal pool with hypermutated new clones
    // try the new weights, derived from the clone, & get the error terms for
    // each clone
    // TODO: network.eval! fine ... (clone trial uses eval ...)
    val newFitness: Array[Double] = newClonalPool
      .map(clone => cloneTrial(clone, network, trainingSet))

    // get the fittest clone
    // TODO: getFittest uses the array derived from network.eval
    val newWeights: Array[Double] = newClonalPool(getFittest(newFitness))

    // now we need to apply the best weights to the network weights
    applySequence2Weights(newWeights, network.layers.toArray)

    newFitness
  }

  // apply the clone to the network weights, evaluate, then return the
  // errorTerms
  def cloneTrial(clone: Array[Double],
                 network: NeuralNetwork,
                 trainingSet: DataSet): Double = {
    //println("SomHypeTrainer: in cloneTrial")
    applySequence2Weights(clone, network.layers.toArray)
    // TODO: have a GOOD LOOK at whether or not we should be using this below ...
    network.eval(trainingSet)
  }


  def getFittest(errors: Array[Double]): Int = {
    //println("SomHypeTrainer: in getFittest")
    import scala.collection.immutable.ListMap
    val m1 = (errors.indices zip errors).toMap
    // TODO: we want the highest score here
    //ListMap(m1.toSeq.sortBy(_._2): _*).head._1
    ListMap(m1.toSeq.sortWith(_._2 > _._2): _*).head._1
  }


  def evalIteration(iteration: Int,
                    network: SomHypeNeuralNetwork,
                    trainingSet: DataSet,
                    batch: DataSet) {
    // TODO: network.eval used here, need to MAXIMISE
    if ((iteration + 1) % evalIterations == 0) {
      val loss = network.loss(trainingSet)
      println("Iteration:%5d, Loss: %.5f, Accuracy: %f".format(iteration + 1,
        loss,
        network.eval(trainingSet)))
    }
  }


  def train(network: SomHypeNeuralNetwork,
            hypermutate: ConstrainedSequenceHypermutation,
            trainingSet: DataSet,
            iterations: Int = 500) = {


    // TODO: look at numIterations here ...
    // TODO: also, if the take function is doing what i think it is, we need
    // TODO: to GET A RANDOM SAMPLE from the data set ...


    /*
    // TODO: let's deconstruct ...
    val t1: Iterator[DataSet] = trainingSet.miniBatches(miniBatchSize)
    val t2: t1.GroupedIterator[DataSet] = t1.grouped(numParallel)
    val t3: Iterator[Seq[DataSet]] = t2.take(iterations)
    val t4: Iterator[(Seq[DataSet], Int)] = t3.zipWithIndex
    */



    trainingSet.
      miniBatches(miniBatchSize).
      grouped(numParallel).
      take(iterations).
      zipWithIndex.foreach {
      case (batches, iteration) =>

        // TODO: right. we need to have a SINGLE ERROR FUNCTION.
        // TODO: we also need to know: MAX OR MIN?

        // TODO: evalIteration == network.eval
        // TODO: network.eval used here, need to MAXIMISE
        evalIteration(iteration, network, trainingSet, batches(0))



        // TODO: BELOW: NOT network.eval, REVIEW THIS! MINIMISE
        //val batchGradients: Seq[DoubleMatrix] = batches.map(network.errorGradients)
        //val batchError = meanBatchError(batchGradients)
        //println(s"$iteration $batchError")
        // TODO: ABOVE: NOT network.eval, REVIEW THIS! MINIMISE

        // TODO: how do we turn a bunch of matrices into e.g. a single error term?


        updateWeights(network, hypermutate, trainingSet, iteration)
    }
  }

  def meanBatchError(xm: Seq[DoubleMatrix]) = {
    xm.map { matrix =>
      matrix.
        data.
        map { elem => Math.abs(elem) }.sum / matrix.data.length
    }
  }.sum / xm.length


}


object SimpleSomHypeTrainer {

  def apply(numIterations: Int,
            miniBatchSize: Int = 100,
            momentumMultiplier: Double = 1.0,
            gradientChecker: Option[GradientChecker] = None,
            evalIterations: Int = 10,
            numParallel: Int = 1) =
    new SimpleSomHypeTrainer(numIterations,
      miniBatchSize,
      if (numParallel < 1) 1 else numParallel,
      momentumMultiplier,
      gradientChecker,
      evalIterations)

}
