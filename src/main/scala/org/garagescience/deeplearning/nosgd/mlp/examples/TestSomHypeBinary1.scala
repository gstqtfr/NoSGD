package org.garagescience.deeplearning.nosgd.mlp.examples

import org.garagescience.deeplearning.nosgd.mlp.data._
import org.garagescience.deeplearning.nosgd.actors._
import org.garagescience.deeplearning.nosgd.actors.binary._
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object TestSomHypeBinary1 {


  private def creatActor(sys: ActorSystem,
                         inputs: Int,
                         outputs: Int
                        ): ActorRef = {
    sys.actorOf(SomHypeBinarySeqActor.props(inputs, outputs, true))
  }


  val trainingSetDir = "data/train"
  val testSetDir = "data/test"

  private final val popSz = 5
  private final val epsilon: Double = 0.01
  private final val numIterations: Int = 500
  private final val miniBatchSize: Int = 50
  private final val evalIterations: Int = 1000


  def main(args: Array[String]): Unit = {

    val trainSet: ImageTileDataSet =
      ImageTileDataSet(
        trainingSetDir,
        numClasses = 10,
        imageDimension = 28)

    println("Training Examples: " + trainSet.numExamples)


    val system = ActorSystem("TestGCmultipleActorSystem")


    val gcl: List[ActorRef] = {
      for {i <- 0 until popSz} yield creatActor(system, trainSet.numInputs, trainSet.numOutputs)
    }.toList


    val gcc: ActorRef = system.
      // TODO: CHANGE ME!!!
      // TODO: okay, maybe not - the code is suff. generic, we
      // TODO: shouldn't need to ...
      actorOf(SomHypeControllerActor.props(
      trainSet,
      gcl,
      epsilon,
      numIterations,
      miniBatchSize,
      evalIterations), name = "gcl")

    gcc ! KickOff
  }

}