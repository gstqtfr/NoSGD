package org.garagescience.deeplearning.nosgd.mlp

import org.garagescience.deeplearning.nosgd.bca.FixedPointHypermutation

import scala.collection.immutable.{Seq => TSeq}
import org.garagescience.deeplearning.nosgd.mlp.data.DataSet
import org.jblas.DoubleMatrix

// TODO: implement momentum & learning rate decay
// TODO: (see https://stats.stackexchange.com/questions/70101/neural-networks-weight-change-momentum-and-weight-decay)

// TODO: so, for momentum, we jsut add the previous weight change * a coefficient

/*
this style of thing:
Momentum αα is used to diminish the fluctuations in weight changes over consecutive iterations:

Δωi(t+1)=−η∂E∂wi+αΔωi(t),
Δωi(t+1)=−η∂E∂wi+αΔωi(t),
where E(w) is the error function, w - the vector of weights, η - learning rate.

// TODO: So why can't we do αΔωi(t) + α^2Δωi(t-1), for e.g.?
 */

// TODO: implement multiple-point mutation


// TODO: the ONLY DIFFERENCE is type of hypermutate!!!
class FixedPointSomHypeInterface(network: SomHypeNeuralNetwork,
                                 seed: TSeq[Double],
                                 verbose: Boolean = true) {

  def me = this.getClass.getSimpleName

  val hypermutate = new FixedPointHypermutation(seed)

  def update(iteration: Int,
             trainingSet: DataSet): TSeq[Double] = {

    val fitnessCollection: TSeq[Double] = (0 until trainingSet.numExamples).map { i =>
      // TODO: network.eval! fine ...
      //val currentFitness = network.eval(trainingSet)
      // TODO: network.errorGradients modification
      val currentFitness: DoubleMatrix = network.errorGradients(trainingSet)
      //  println(s"$iteration $currentFitness")

      // produce the next generation of hypermutated clones
      val newClonalPool: TSeq[TSeq[Double]] = hypermutate.germinate

      // update the clonal pool with hypermutated new clones
      // try the new weights, derived from the clone, & get the error terms for
      // each clone
      // TODO: network.eval! fine ... (clone trial uses eval ...)
      // TODO: network.errorGradients modification
      val newFitness: TSeq[Double] = newClonalPool
        .map(clone => cloneTrial(clone, network, trainingSet))

      // get the fittest clone
      // TODO: getFittest uses the TSeq derived from network.eval
      // TODO: network.errorGradients modification
      val newWeights: TSeq[Double] = newClonalPool(getFittest(newFitness))

      // now we need to apply the best weights to the network weights
      applySequence2Weights(newWeights, network.layers)

      newFitness.sum  / newFitness.length
    }

    fitnessCollection
  }

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
    //println("SomHypeTrainer: in cloneTrial")
    applySequence2Weights(clone, network.layers)
    // TODO: have a GOOD LOOK at whether or not we should be using this below ...
    //network.eval(trainingSet)
    // this is the Frobenius norm of the error matrix
    network.errorGradients(trainingSet).norm2()
  }

  private def getFittest(errors: TSeq[Double]): Int = {
    //println("SomHypeTrainer: in getFittest")
    import scala.collection.immutable.ListMap
    val m1 = (errors.indices zip errors).toMap
    // so, we have the Frobenius norm, we NEED TO MINIMISE!
    ListMap(m1.toSeq.sortWith(_._2 < _._2): _*).head._1
  }


  private def getLeastFittest(errors: TSeq[Double]): Int = {
    //println("SomHypeTrainer: in getFittest")
    import scala.collection.immutable.ListMap
    val m1 = (errors.indices zip errors).toMap
    // so, we have the Frobenius norm, we NEED TO MINIMISE!
    ListMap(m1.toSeq.sortWith(_._2 > _._2): _*).head._1
  }

}