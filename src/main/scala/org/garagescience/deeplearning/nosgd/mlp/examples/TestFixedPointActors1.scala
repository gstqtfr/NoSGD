package org.garagescience.deeplearning.nosgd.mlp.examples

import org.garagescience.deeplearning.nosgd.mlp._
import org.garagescience.deeplearning.nosgd.mlp.data._
import org.garagescience.deeplearning.nosgd.actors._
import akka.actor.{Actor, ActorRef, ActorSystem, Props}


object TestFixedPointActors1 {

  private def creatActor(sys: ActorSystem,
                         inputs: Int,
                         outputs: Int
                        ): ActorRef = {
    sys.actorOf(FixedPointNetActor.props(inputs, outputs))
  }

  def me = this.getClass.getSimpleName

  val trainingSetDir = "data/train"
  val testSetDir = "data/test"

  private final val popSz = 10
  private final val epsilon: Double = 0.01
  private final val numIterations: Int = 1000
  private final val miniBatchSize: Int = 10
  private final val evalIterations: Int = 100


  def main(args: Array[String]): Unit = {


    val trainSet: ImageTileDataSet = ImageTileDataSet(
      trainingSetDir,
      numClasses = 10,
      imageDimension = 28)

    println("Training Examples: " + trainSet.numExamples)

    val system = ActorSystem("TestFPMultipleActorSytem")

    val gcl: List[ActorRef] = {
      for {i <- 0 until popSz} yield creatActor(system, trainSet.numInputs, trainSet.numOutputs)
    }.toList

    println(s"$me: created FixedPointNetActor actors")

    val gcc: ActorRef = system.
      actorOf(SomHypeControllerActor.props(
        trainSet, gcl, epsilon, numIterations, miniBatchSize, evalIterations
      ), name = "gcl")

    println(s"$me: created SomHypeControllerActor, sending KickOff ... ")

    gcc ! KickOff


  }

}