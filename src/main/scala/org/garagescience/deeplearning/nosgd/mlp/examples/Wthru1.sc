// sbt -mem 3g console

import org.garagescience.deeplearning.nosgd.linalg._
import org.garagescience.deeplearning.nosgd.mlp._
import org.garagescience.deeplearning.nosgd.bca._
import org.garagescience.deeplearning.nosgd.mlp.SomHypeNeuralNetwork
import org.garagescience.deeplearning.nosgd.mlp.data.ImageTileDataSet
import org.garagescience.deeplearning.nosgd.mlp.data._
import org.jblas.DoubleMatrix

val trainingSetDir = "data/train"
val testSetDir = "data/test"
val trainSet = ImageTileDataSet(trainingSetDir, numClasses = 10, imageDimension = 28)

val network = BackPropNeuralNetwork(
  Layer(trainSet.numInputs, 100, Logistic) :+ Layer(trainSet.numOutputs, SoftMax),
  objective = CrossEntropyError,
  weightDecay = 0.0)




val mutant = SomHypeNeuralNetwork(
  Layer(trainSet.numInputs, 100, Logistic) :+ Layer(trainSet.numOutputs, SoftMax),
  objective = CrossEntropyError)



// TODO: break this down. so:

val it=trainSet.miniBatches(100)
// it: Iterator[org.garagescience.deeplearning.nosgd.mlp.data.DataSet] = non-empty iterator




val performance = trainSet.miniBatches(100).take(50).
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



// FIXME: right. choose one of these metrics - e.g. loss
// FIXME: do the hypermutation thing
// FIXME: see if it works okay.

// FIXME: do this w'out actors for now, then actor it up when you're happy ...


trainSet.miniBatches(10).take(50).
  zipWithIndex.foreach { case (batches: DataSet, iteration: Int) =>

  println(s"Iteration          : $iteration")


}



// this is also nifty:
// gets us a dataset ...

val tmp = trainSet.miniBatches(1).take(1).toList.head

/*
// TODO:

right. so what we have is inputs, which is a double matrix, targets, which is a
vector (DoubleMatrix) with ten columns, this style of thing:

[0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0]

er ... that's it ... so it looks pretty simple ...

so, now we need to

a) get a network being presented with the patterns
b) get the errors ...

 */

// trainSet.miniBatches(1).toList


/*

(0 until trainSet.numExamples).map { i =>

*/