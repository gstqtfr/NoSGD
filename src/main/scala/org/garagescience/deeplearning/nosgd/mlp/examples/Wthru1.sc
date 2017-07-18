// sbt -mem 3g console

import org.garagescience.deeplearning.nosgd.linalg._
import org.garagescience.deeplearning.nosgd.mlp._
import org.garagescience.deeplearning.nosgd.bca._
import org.garagescience.deeplearning.nosgd.mlp.data.ImageTileDataSet

val trainingSetDir = "data/train"
val testSetDir = "data/test"
val trainSet = ImageTileDataSet(trainingSetDir, numClasses = 10, imageDimension = 28)

val network = BackPropNeuralNetwork(
  Layer(trainSet.numInputs, 100, Logistic):+Layer(trainSet.numOutputs, SoftMax),
  objective = CrossEntropyError,
  weightDecay = 0.0)



