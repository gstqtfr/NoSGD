package org.garagescience.deeplearning.nosgd.actors

import akka.actor.{Actor, Props}
import org.garagescience.deeplearning.nosgd.layerlevel.LayersGerminalCentre
import org.garagescience.deeplearning.nosgd.mlp._
import org.garagescience.deeplearning.nosgd.mlp.NeuralNetwork._
import org.garagescience.deeplearning.nosgd.mlp.data.DataSet
import org.jblas.DoubleMatrix

import scala.collection.immutable.{IndexedSeq, ListMap}
import scala.language.postfixOps

class SomHypeNetActorWithAddedMo(numInputs: Int,
                                 numOutputs: Int,
                                 alpha: Double = 0.01,
                                 eta: Double = 0.001,
                                 poolSize: Int = 10,
                                 verbose: Boolean = true)
  extends ThinController(eta) with Actor {


  // FIXME: do we make this a clonal pool of networks?!?
  // FIXME: a SMALL pool of networks? or do we use one clone
  // FIXME: per actor?! i.e. we have a pool of actors?!

  // FIXME: this would make it easier to parallelise/distribute

  // FIXME: so: should we be passing the layers (weights) around instead?!

  // here's our own, personal, neural network; nobody else's. it OURS ...
  // FIXME: 100?!?
  // this is the poor neural network which is going to get regularly abused
  // below ...
  var mutant = new SomHypeNeuralNetwork(
    Layer(numInputs, 100, Logistic) :+ Layer(numOutputs, SoftMax),
    objective = CrossEntropyError)

  def update(net: SomHypeNeuralNetwork,
             iteration: Int,
             trainingSet: DataSet) = {

    // we want to minimise the loss, as returned from evaluate above
    val currentLoss: Double = evaluate(net, trainingSet)

    // save the current weights ...
    val currentLayers: List[Layer] = net.layers


    // hypermutate the weights

    val lgc = new LayersGerminalCentre(currentLayers, alpha, eta, true)

    // this creates the clonal pool, & mutates at the same time
    val clonalPool: List[List[Layer]] = {
      // FIXME: also, need to include memory
      for {i <- 0 until poolSize} yield lgc.germinate
    }.toList

    // performance of the clonal pool
    val clonalLoss: List[Double] = loadAndEvaluate(net, clonalPool, trainingSet)

    // compare performance, get the best
    // we want to minimise loss

    val sortedClonalLoss: ListMap[Int, Double] = sortByFitness(clonalLoss)
    val cloneWithLowestLoss = sortedClonalLoss.head._2
    val cloneIndex = sortedClonalLoss.head._1

    if (cloneWithLowestLoss < currentLoss) {
      // make sure the best performing weights are loaded ...
      net.copy(clonalPool(cloneIndex))
    } else {
      // TODO: do we NEED to do this? or just return the net as is?
      net.copy(currentLayers)
    }

  }

  def sortByFitness(_loss: List[Double]): ListMap[Int, Double] = {
    import scala.collection.immutable.ListMap
    val m1 = (_loss.indices zip _loss).toMap
    ListMap(m1.toSeq.sortWith(_._2 < _._2): _*)
  }

  def evaluate(net: SomHypeNeuralNetwork,
               trainingSet: DataSet): Double = net.loss(trainingSet)

  // FIXME: memory cell!!!
  def loadAndEvaluate(net: SomHypeNeuralNetwork,
                      pool: List[List[Layer]],
                      trainingSet: DataSet): List[Double] = {

    def _loadAndEvaluate(_pool: List[List[Layer]],
                         l: List[Double]): List[Double] = _pool match {
      case Nil => l
      case layers :: morelayers =>
        // load up the layers - this gives us a new network with the
        // hypermutated weights from the clonal pool
        val _net = net.copy(layers)
        _loadAndEvaluate(morelayers, evaluate(_net, trainingSet) :: l)
    }

    _loadAndEvaluate(pool, List())
  }


  //


  // FIXME: so we need to implement this jobby below, preferably one
  // FIXME: batch at a time ...

  // FIXME: & miniBatches will return an iterator (over a stream)
  // FIXME: so we can get a mini-batch & do the hasNext/next thing ...
  /*
  val performance = trainSet.miniBatches(100).take(3000).
  zipWithIndex.map {
  case (batches: DataSet, iteration: Int) =>
    println(s"Iteration          : $iteration")
    //println(s"Columns (examples) : ${batches.numExamples}")
    //println(s"Rows (inputs)      : ${batches.numInputs}")
    //println(s"Rows (target)      : ${batches.numOutputs}")
    // TODO: let's iterate through this fella ...
    val dataset = batches.miniBatches(1).take(1).toList.head
    //println(s"DS examples        : ${dataset.numExamples}")
    //println(s"DS inputs          : ${dataset.numInputs}")
    //println(s"DS inputs          : ${dataset.numOutputs}")
    println(s"DS targets         : ${dataset.targets}")
    val gradients: List[DoubleMatrix] = network.errorGradients(batches).toList
    val loss: Double = network.loss(batches)
    val eval: Double = network.eval(batches)
    println(s"Loss               : $loss")
    println(s"Eval               : $eval")
    for (i <- 0 until gradients.length) {
      val gradm = gradients(i).norm2()
      println(s"Gradient           : $gradm")
    }
    println(s"")
    println()
    (loss, eval)
}.toList
   */


  /*

  def update(iteration: Int,
             trainingSet: DataSet) = {

    // FIXME: this can be refactored to be more general, let's do that!
    // FIXME: let's finish the logic 1st ...

    val currentFitness = (0 until trainingSet.numExamples).map { i =>
      // we get the current error
      val currentFitness: Double = evaluate(mutant, trainingSet)
      mutant.errorGradients(trainingSet).norm2()
    }

    // produce the next generation hypermutated clone
    val lgc: List[Layer] = new LayersGerminalCentre(mutant.layers, alpha, eta, true).germinate

    // load it up to a network, so that we can eval. it


    val nextFitness = (0 until trainingSet.numExamples).map { i =>



      // evaluate the clone
      val cloneFitness = evaluate(trainingSet, Some(lgc))

    }


  }

  */

  /*


    val newFitness: Array[Double] = newClonalPool
      .map(clone => cloneTrial(clone, network, trainingSet))

    // get the fittest clone
    // TODO: getFittest uses the array derived from network.eval
    // TODO: network.errorGradients modification


    val newWeights: Array[Double] = newClonalPool(getFittest(newFitness))

    // now we need to apply the best weights to the network weights
    applySequence2Weights(newWeights, network.layers.toArray)

    newFitness.sum / newFitness.length

    */

  def receive = {

    case ThisDataGC(iteration, data) =>
      if (verbose) log.info(s"${self.path} received DataGC")
      // TODO: we present the data to the network
      // TODO: we get the error
      // TODO: we mutate those suckers
      // TODO: we get the NEW error
      // TODO: we get the least error
      // TODO: we return it to the controller actor

      // FIXME: let's have some real errors ...
      val errors = Array(0.0)
      sender ! TheseErrorsGC(iteration, errors)


    case FinalWhistle =>
      log.info(s"${self.path} received FinalWhistle, shutting down")
      context.stop(self)


    case m =>
      log.info(s"${self.path} received unknown message from ${sender}, message ${m}")

  }


}


object SomHypeNetActorWithAddedMo {

  // best practise is to put the props close to where the
  // actor itself is init'd
  def props(numInputs: Int, numOutputs: Int): Props =
  Props(new SomHypeNetActorWithAddedMo(numInputs, numOutputs))

  def props(numInputs: Int, numOutputs: Int, alpha: Double): Props =
    Props(new SomHypeNetActorWithAddedMo(numInputs, numOutputs, alpha))

  def props(numInputs: Int, numOutputs: Int, alpha: Double, eta: Double): Props =
    Props(new SomHypeNetActorWithAddedMo(numInputs, numOutputs, alpha, eta))

  // TODO: add the boolean
  /*
  def props(numInputs: Int, numOutputs: Int, alpha: Double, eta: Double, boolean: Boolean): Props =
    Props(new SomHypeNetActorWithAddedMo(numInputs, numOutputs, alpha, eta, boolean))
    */
}

