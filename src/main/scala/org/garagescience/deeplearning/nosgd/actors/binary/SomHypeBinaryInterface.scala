package org.garagescience.deeplearning.nosgd.actors.binary

import org.garagescience.deeplearning.nosgd.bca.binary.{BinarySequencePointHypermutation, BinaryVectorGerminalCentre}

import scala.collection.immutable.{IndexedSeq, Seq => TSeq}
import org.garagescience.deeplearning.nosgd.mlp.{Layer, SomHypeNeuralNetwork}
import org.garagescience.deeplearning.nosgd.mlp.data.DataSet
import org.jblas.DoubleMatrix

// FIXME: we can make this generic quite easily
// FIXME: also, we can abstract this to a trait

class SomHypeBinaryInterface(network: SomHypeNeuralNetwork,
                            seed: TSeq[Double]) {

  // TODO: we could pass this as a param using a trait, then the whole
  // TODO: code base would become easier to make general
  val hypermutate = new BinaryVectorGerminalCentre(seed)

  def applySequence2Weights(clone: TSeq[Double],
                            layers: TSeq[Layer]) = {

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

  private def cloneTrial(clone: TSeq[Double],
                         network: SomHypeNeuralNetwork,
                         trainingSet: DataSet): Double = {
    println("SomHypeTrainer: in cloneTrial")
    applySequence2Weights(clone, network.layers)
    // TODO: have a GOOD LOOK at whether or not we should be using this below ...
    //network.eval(trainingSet)
    // this is the Frobenius norm of the error matrix
    network.errorGradients(trainingSet).norm2()
  }

  private def getFittest(errors: TSeq[Double]): Int = {
    println("SomHypeTrainer: in getFittest")
    import scala.collection.immutable.ListMap
    val m1 = (errors.indices zip errors).toMap
    // FIXME: this gives you the scores by lowest:
    //ListMap(m1.toSeq.sortBy(_._2): _*).head._1
    // FIXME: ... & so does this:
    // so, we have the Frobenius norm, we NEED TO MINIMISE!
    ListMap(m1.toSeq.sortWith(_._2 < _._2): _*).head._1
    // ... but we want the highest score here
    // ListMap(m1.toSeq.sortWith(_._2 > _._2): _*).head._1
  }

  def update(iteration: Int,
             trainingSet: DataSet): TSeq[Double] = {

    val fitnessCollection: TSeq[Double] = (0 until trainingSet.numExamples).map { i =>
      // TODO: network.eval! fine ...
      //val currentFitness = network.eval(trainingSet)
      // TODO: network.errorGradients modification
      val currentFitness: DoubleMatrix = network.errorGradients(trainingSet)
      println(s"$iteration $currentFitness")

      // produce the next generation of hypermutated clones
      val newClonalPool: TSeq[TSeq[Double]] = hypermutate.iterate

      // update the clonal pool with hypermutated new clones
      // try the new weights, derived from the clone, & get the error terms for
      // each clone
      // TODO: network.eval! fine ... (clone trial uses eval ...)
      // TODO: network.errorGradients modification
      val newFitness: TSeq[Double] = newClonalPool
        .map(clone => cloneTrial(clone, network, trainingSet))

      // get the fittest clone
      // TODO: getFittest uses the array derived from network.eval
      // TODO: network.errorGradients modification
      val newWeights: TSeq[Double] = newClonalPool(getFittest(newFitness))

      // now we need to apply the best weights to the network weights
      applySequence2Weights(newWeights, network.layers)

      newFitness.sum / newFitness.length
    }

    fitnessCollection
  }

}