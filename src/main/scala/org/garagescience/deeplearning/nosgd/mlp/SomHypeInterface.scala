package org.garagescience.deeplearning.nosgd.mlp

import org.garagescience.deeplearning.nosgd.bca.ConstrainedSequenceHypermutation
import org.garagescience.deeplearning.nosgd.mlp.data.DataSet
import org.jblas.DoubleMatrix

// TODO: OPTIMISATION IDEAS:
// TODO: try MULIPLE HOTSPOT MUTATION!
// TODO: create a mask so that only x \in [-1,1] can be produced
// TODO: this way we only work on the correct range of bit patterns ...
// TODO: restrict most activity to the mantissa


// TODO: check out: http://www.panojohnson.com/automatons/floating-point-1.xhtml
// TODO: you can click on e.g. mantissa, exponent & see the bit pattern



// TODO: using the site above, REDO so we get double-precision fp!!!





// TODO: so it should be RELATIVELY EASY to create something which is
// TODO: constrained between these values, just working on the mantissa

  /*


 0.5:
±	Exponent	Mantissa
0	0	1	1	1	1	1	1	1	1	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
 -0.5
±	Exponent	Mantissa
1	0	1	1	1	1	1	1	1	1	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0

 If we freeze the exponent as is, we can get arbitrarily close to -1.0 / 1.0

  So, the exponent (including the sign bit) we want is:


±	Exponent
0	0	1	1	1	1	1	1	1	1	1	0

  or

±	Exponent
1	0	1	1	1	1	1	1	1	1	1	0

// TODO:

// usual: random length, mutate mantissa & sign bit, LEAVE EXPONENT
// always start at 0! random length, mutate mantissa


  furthermore, we could freeze the exponent to a small number, e.g.

  positive: 001111110    (N.B. 01111110 = 126)
  negative: 101111110    (N.B. the leading 1, the sign bit, is sep. to the exponent)

  SO, that

  */

class SomHypeInterface(network: SomHypeNeuralNetwork,
                       // initial values for the germinal centre
                       seed: Array[Double]) {

  // TODO: is there any reason this class can't creat its own network?

  val hypermutate = new ConstrainedSequenceHypermutation(seed)

  def update(iteration: Int,
              trainingSet: DataSet): Array[Double] = {

    val fitnessCollection: Array[Double] = (0 until trainingSet.numExamples).map { i =>
      // TODO: network.eval! fine ...
      //val currentFitness = network.eval(trainingSet)
      // TODO: network.errorGradients modification
      val currentFitness: DoubleMatrix = network.errorGradients(trainingSet)
     //  println(s"$iteration $currentFitness")

      // produce the next generation of hypermutated clones
      val newClonalPool: Array[Array[Double]] = hypermutate.germinate

      // update the clonal pool with hypermutated new clones
      // try the new weights, derived from the clone, & get the error terms for
      // each clone
      // TODO: network.eval! fine ... (clone trial uses eval ...)
      // TODO: network.errorGradients modification
      val newFitness: Array[Double] = newClonalPool
        .map(clone => cloneTrial(clone, network, trainingSet))

      // get the fittest clone
      // TODO: getFittest uses the array derived from network.eval
      // TODO: network.errorGradients modification
      val newWeights: Array[Double] = newClonalPool(getFittest(newFitness))

      // now we need to apply the best weights to the network weights
      applySequence2Weights(newWeights, network.layers.toArray)

      newFitness.sum / newFitness.length
    }.toArray

    fitnessCollection
  }

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

  private def cloneTrial(clone: Array[Double],
                         network: SomHypeNeuralNetwork,
                         trainingSet: DataSet): Double = {
    //println("SomHypeTrainer: in cloneTrial")
    applySequence2Weights(clone, network.layers.toArray)
    // TODO: have a GOOD LOOK at whether or not we should be using this below ...
    //network.eval(trainingSet)
    // this is the Frobenius norm of the error matrix
    network.errorGradients(trainingSet).norm2()
  }

  private def getFittest(errors: Array[Double]): Int = {
    //println("SomHypeTrainer: in getFittest")
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
}