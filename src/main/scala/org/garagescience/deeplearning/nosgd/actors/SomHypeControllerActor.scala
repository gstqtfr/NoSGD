package org.garagescience.deeplearning.nosgd.actors

import akka.actor.{Actor, ActorRef, Props}
import org.garagescience.deeplearning.nosgd.mlp.data.DataSet
import scala.language.postfixOps

// TODO: WE NEED A MORE SENSITIVE ERROR FUNCTION!

// TODO: test what happens if we > 1 of these!
// TODO: *lots* of parallelism

class SomHypeControllerActor(trainset: DataSet,
                             // a supervisor actor would create its own, rather than
                             // have it passed
                             _gcl: List[ActorRef],
                             epsilon: Double = 0.01,
                             numIterations: Int = 300,
                             miniBatchSize: Int = 10,
                             evalIterations: Int = 10,
                             verbose: Boolean = true
                            ) extends ThinController(epsilon) {


  var gcl = _gcl

  def sendToAll(msg: GCMessages, actor: List[ActorRef]) = {

    def _sendToAll(_msg: GCMessages, _actor: List[ActorRef]): List[ActorRef] = _actor match {
      case Nil => _actor
      case h :: t =>
        h ! _msg
        _sendToAll(_msg, t)
    }

    _sendToAll(msg, actor)
  }


  def killAllTheOthers(actor: List[ActorRef]) = {

    def _killAllTheOthers(_actor: List[ActorRef]): List[ActorRef] = _actor match {
      case Nil => _actor
      case h :: t =>
        context.stop(h)
        _killAllTheOthers(t)
    }

    _killAllTheOthers(actor)
  }

  // this gets a specific example (i.e. column) from the data
  def getDataSpecificExample(it: Int): ThisDataGC =
    ThisDataGC(it, trainset.batch(Array(it)))

  // gets an array of random numbers constrained by the sz parameter
  def getRandomArray(sz: Int): Array[Int] =
    (0 until sz).map { i =>
      scala.util.Random.nextInt(trainset.numExamples)
    }.toArray

  // this gets a sample, randomly, from the data, with replacement
  def getRandomSample(sz: Int) =
    trainset.batch(Array(sz))



  def receive = {


    case KickOff =>
      log.info(s"${self.path} received kickOff")
      // give our actors a batch of the data
      // TODO: need to investigate how big the batch should be
      sendToAll(ThisDataGC(0, getRandomSample(miniBatchSize)), gcl)



    // if an actor has sent ErrorsGC, it means they've completed the data
    case TheseErrorsGC(it, errors) =>
      if (verbose) {
        log.info(s"${self.path} ${count} received TheseErrorsGC")
        log.info(s"${self.path} ${count} errors: ${errors}")
        log.info(s"${self.path} ${count} error term: ${count} : ${getMinimum(errors)}")
      } else {
        // log.info(s"$it ${getMinimum(errors)}")
        println(s"$it ${getMean(errors)}")
      }

      if (it == numIterations) {
        // don't kill it yet - we need the fittest clone
        // after we get the fittest clone, then we kill it
        gcl = gcl.filterNot(_ == sender)
        sender ! FinalWhistle
        if (gcl.length == 0) {
          log.info(s"${self.path} ${count} committing suicide - goodbye cruel world ...")
          context.stop(self)
        }
      } else {
        // send the next batch of data
        sender ! getDataSpecificExample(it + 1)
      }

  }

}

object SomHypeControllerActor {

  def props(trainset: DataSet,
            // a supervisor actor would create its own, rather than
            // have it passed
            _gcl: List[ActorRef],
            epsilon: Double,
            numIterations: Int,
            miniBatchSize: Int,
            evalIterations: Int): Props =
    Props(new SomHypeControllerActor(
      trainset,
      _gcl,
      epsilon,
      numIterations,
      miniBatchSize,
      evalIterations)
    )

}

