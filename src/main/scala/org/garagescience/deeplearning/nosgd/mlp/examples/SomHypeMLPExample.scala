package org.garagescience.deeplearning.nosgd.mlp.training

import org.garagescience.deeplearning.nosgd.bca.ConstrainedSequenceHypermutation
import org.garagescience.deeplearning.nosgd.mlp._
import org.garagescience.deeplearning.nosgd.mlp.data._
import org.garagescience.deeplearning.nosgd.training.SimpleSomHypeTrainer
import org.garagescience.deeplearning.nosgd.mlp.NeuralNetwork._

object SomHypeMLPExample {

  val trainingSetDir = "data/train"
  val testSetDir = "data/test"


  def main(args: Array[String]): Unit = {

    val trainSet: ImageTileDataSet = ImageTileDataSet(
      trainingSetDir,
      numClasses = 10,
      imageDimension = 28)



    println("Training Examples: " + trainSet.numExamples)

    val network = new SomHypeNeuralNetwork(
      Layer(trainSet.numInputs, 100, Logistic):+Layer(trainSet.numOutputs, SoftMax),
      objective = CrossEntropyError)

    // get the weights as a sequence, so we can initialise the hypermutator!
    val weights: Array[Double] = weights2Sequence(network)

    val hypermutate = new ConstrainedSequenceHypermutation(weights)

    println("Initial Classification Accuracy: %f".format(network.eval(trainSet)))

    val trainer = SimpleSomHypeTrainer(
      numIterations = 3000,
      miniBatchSize = 100,
      momentumMultiplier = 0.9,
      gradientChecker = None, //To check gradients try: Some(GradientChecker(numChecks = 10, accuracy = 8))
      //Some(GradientChecker(numChecks = 10, accuracy = 8)),
      evalIterations = 1000)

    trainer.train(network, hypermutate, trainSet)

    val testSet = ImageTileDataSet(testSetDir, numClasses = 10, imageDimension = 28)

    println("Test Examples: " + testSet.numExamples)
    println("Test Classification Accuracy: %f".format(network.eval(testSet)))

  }

}