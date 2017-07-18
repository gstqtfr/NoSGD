package org.garagescience.deeplearning.nosgd.mlp.examples

import org.garagescience.deeplearning.nosgd.bca.ConstrainedSequenceHypermutation
import org.garagescience.deeplearning.nosgd.mlp._
import org.garagescience.deeplearning.nosgd.mlp.data._
import org.garagescience.deeplearning.nosgd.actors._
import org.garagescience.deeplearning.nosgd.training.SimpleSomHypeTrainer
import org.garagescience.deeplearning.nosgd.mlp.NeuralNetwork._
import akka.actor.{Actor, ActorRef, ActorSystem, Props}


object TestSomHypeActors1 {



  private def creatActor(sys: ActorSystem,
                         inputs: Int,
                         outputs: Int
                        ): ActorRef = {
    sys.actorOf(SomHypeNetActor.props(inputs, outputs))
  }


  val trainingSetDir = "data/train"
  val testSetDir = "data/test"

  private final val popSz = 20
  private final val epsilon: Double = 0.01
  private final val numIterations: Int = 500
  private final val miniBatchSize: Int = 50
  private final val evalIterations: Int = 1000

  def main(args: Array[String]): Unit = {


    val trainSet: ImageTileDataSet = ImageTileDataSet(
      trainingSetDir,
      numClasses = 10,
      imageDimension = 28)

    println("Training Examples: " + trainSet.numExamples)

    val system = ActorSystem("TestGCmultipleActorSytem")

    val gcl: List[ActorRef] = {
      for {i <- 0 until popSz} yield creatActor(system, trainSet.numInputs, trainSet.numOutputs)
    }.toList


    val gcc: ActorRef = system.
      actorOf(SomHypeControllerActor.props(
        trainSet, gcl, epsilon, numIterations, miniBatchSize, evalIterations
      ), name = "gcl")

    gcc ! KickOff

  }
}