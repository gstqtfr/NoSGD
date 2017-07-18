import org.garagescience.deeplearning.nosgd.mlp.training._
import org.garagescience.deeplearning.nosgd.bca.ConstrainedSequenceHypermutation
import org.garagescience.deeplearning.nosgd.mlp._
import org.garagescience.deeplearning.nosgd.mlp.data._
import org.garagescience.deeplearning.nosgd.training.SimpleSomHypeTrainer
import org.garagescience.deeplearning.nosgd.mlp.NeuralNetwork._

// TODO: ALL that the data set is is a pair of matrices
// TODO: inputs & targets

// TODO: the number of examples is given by inputs.columns
// TODO: the number of inputs is given by   inputs.rows
// TODO: the number of outputs is given by  targets.rows

// TODO: er, that's it ...


// okay, so we have
// trainSet.inputs.columns == trainSet.numExamples
// that is, each column is an example!


val trainingSetDir = "data/train"
val testSetDir = "data/test"
val trainSet: ImageTileDataSet = ImageTileDataSet(
  trainingSetDir,
  numClasses = 10,
  imageDimension = 28)

// so, to iterate through the data set:
val ds1 = trainSet.batch(Array(0))
val ds2 = trainSet.batch(Array(1))
// u.s.w. ...

def printInfo(dataset: DataSet,
              miniBatchSize: Int = 5,
              numParallel: Int = 1,
              iterations: Int = 5) = {

  val t1: Iterator[DataSet] = dataset.miniBatches(miniBatchSize)

  val t2: t1.GroupedIterator[DataSet] = t1.grouped(numParallel)

  val t3: Iterator[Seq[DataSet]] = t2.take(iterations)
  val t4: Iterator[(Seq[DataSet], Int)] = t3.zipWithIndex

  t4.foreach { case (batches, iteration) =>
    println(s"iteration: $iteration")
    val numExamples = batches(0).inputs.columns
    val numInputs = batches(0).inputs.rows
    val numOutputs = batches(0).targets.rows
    println(s"number of examples: $numExamples")
    println(s"number of inputs:   $numInputs")
    println(s"number of outputs:  $numOutputs")
  }
}