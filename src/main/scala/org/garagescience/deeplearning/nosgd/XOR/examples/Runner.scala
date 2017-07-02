package org.garagescience.deeplearning.nosgd.XOR.examples

import java.lang.System.out.println

import scala.Array.canBuildFrom
import scala.collection.immutable.HashMap
import scala.io.Source.fromFile
import scala.util.Random

import org.garagescience.deeplearning.nosgd.XOR.SigmoidFunction
import org.garagescience.deeplearning.nosgd.XOR.FeedForwardNeuralNetwork
import org.garagescience.deeplearning.nosgd.XOR.NeuralNetwork


object Runner {

  import Helpers._
  import scala.collection.immutable.Seq

  /**
    * a helper class for generating sequences consisting of zeros and ones, generating all of them, generating
    * random correct examples and verifying their parity
    */
  class ZeroOneSequence(length: Int) {
    def nextRandom: Seq[Int] = {
      Range(0, length) map { _ => if(Helpers.assymetricalCoin(0.5)) 1 else 0}
    }

    lazy val zeros: Seq[Int] = Range(0, length).map(_ => 0)
    lazy val ones: Seq[Int] = Range(0, length).map(_ => 1)

    lazy val combinationCount: Int = 1 << length

    def allSequences: Iterator[Seq[Int]] = new ZeroOneSequence.PossibleSequencesIterator(length)
  }

  object ZeroOneSequence {
    /**
      * 0 - even, 1 - odd
      */
    def checkParity(s: Seq[Int]): Int = {
      assert(s.forall(p => p == 1 || p == 0))
      s.sum % 2
    }

    private class PossibleSequencesIterator(length: Int) extends Iterator[Seq[Int]] {
      private var _lastSeqO: Option[Seq[Int]] = None

      def hasNext: Boolean = _lastSeqO match {
        case None => true
        case Some(s) => {
          ! s.forall(_ == 1)
        }
      }
      def next =  _lastSeqO match {
        case None => {
          val newLast: Seq[Int] = Range(0, length).map(_ => 0)
          _lastSeqO = Some(newLast)
          newLast
        }
        case Some(s) => {
          var carry = 1; //adding the one
          val newLast = s.map { i =>
            val j = i + carry
            carry = j / 2
            j % 2
          }
          _lastSeqO = Some(newLast)
          newLast
        }
      }
    }
  }
  def main(args: Array[String]): Unit = {
    time {
      //choose the example you want to run
      //irisExample()
      xorExample()
      //parityExample()
    }
  }

  def irisExample(): Unit = {

    /*
     * reading in the iris data
     */
    val lines = fromFile("example_data/iris.data").getLines.toArray.map(_.split(","))
    type Iris = Tuple5[Double, Double, Double, Double, String]
    def getCoordinates(sr: Array[String]): Array[Double] = {
      assert(sr.size == 5)
      Array(sr(0).toDouble, sr(1).toDouble, sr(2).toDouble, sr(3).toDouble)
    }

    var coords: Array[Array[Double]] = lines.map(getCoordinates(_))
    val labels: Array[String] = lines.map(l => l(4))

    /*
     * normalizing the iris data
     */
    for(idx <- Range(0, 3).inclusive) {
      val min = coords.map(characteristic => characteristic(idx)).min
      val max = coords.map(characteristic => characteristic(idx)).max

      val spread = max - min

      coords.map(arr => arr(idx) = (arr(idx) - min) / spread )
    }

    /*
     * converting text labels to 1-0 vectors
     */
    val labelMap = HashMap[String, List[Double]](("Iris-setosa", List(1.0, 0.0, 0.0)),
      ("Iris-versicolor", List(0.0, 1.0, 0.0)),
      ("Iris-virginica", List(0.0, 0.0, 1.0)))
    val classificationVectors: Array[List[Double]] = labels.map(l => labelMap(l))

    /*
     * now we have `coords` as sequences of normalized Doubles and `classificationVectors` as sequences of
     * classification sequences with 1.0 at one of the positions
     *
     * choosing training and testing data
     */

    val r = new Random()

    val pairs = r.shuffle(coords.zip(classificationVectors).toSeq)

    //the simplest, imperfect way of verification - we split the example set into training and testing sets
    val (testing, training) = pairs.splitAt(10)

    /*
     * training the nn
     */
    val sf = new SigmoidFunction(1.5)
    val nn = new FeedForwardNeuralNetwork(List(4, 8, 8, 3), sf, 0.4)

    while(nn.getMaxDelta > 0.0001) for(train <- training) {
      nn.train(train._1, train._2)
    }

    /*
     * testing the nn
     */
    val results = for(test <- testing) yield {
      nn.classify(test._1).map(sf.customRound(_)) == test._2.map(sf.customRound(_))
    }

    val successCount = results.count(r => r)

    println("upon testing the neural network got %d/10 results right" format (successCount))


  }

  /**
    * A classic, small example of a neural network is a network is a network calculating the XOR
    * function. A network in the simplest form is not able to do it - we need a network with
    * at least two neurons in the hidden layer. Let's see how it works.
    *
    * Note: this problem, despite its apparent simplicity is
    */
  def xorExample(): Unit = {

    //we will be using the boolean interface, hence the boolean values
    val possibleInputs = List(List(false, false),
      List(false, true),
      List(true, false),
      List(true, true))

    /*
     * List of neuron counts for each layer:
     * two in the input layer, two in the hidden layer, one in the output layer
     *
     * the bias neurons are hidden "under the hood"
     */
    val neuronsInLayers = List(2, 3, 1)

    val sigmoid = new SigmoidFunction(1.9)

    val gamma = 0.8
    //creating the Neural network
    val xornn: NeuralNetwork = new FeedForwardNeuralNetwork(neuronsInLayers, sigmoid, gamma)

    /**
      * now let's see how many times we have to teach the network all the examples in order for it
      * to calculate the xor correctly
      */

    var it: Int = 0
    var correctAnswersCount: Int = 0

    val r = new Random()
    while(correctAnswersCount != 4 && it <= 10000) {
      //teaching the nn
      for(ex <- possibleInputs) {
        xornn.trainBool(ex, Seq(ex.head ^ ex.last))
      }

      //checking if it gives the correct answers already
      // TODO: this is horrid - change it
      val correctAnswers = for (
        pi <- possibleInputs;
        answer = xornn.classifyBool(pi).head;
        correctAnswer = pi.head ^ pi.last
      ) yield answer == correctAnswer

      correctAnswersCount = correctAnswers.count(b => b)
      System.out.println("%d iteration: %d correct answers" format (it, correctAnswersCount))
      it += 1;
    }

    if(it > 10000){
      System.out.println("the system has reached a local optimum without hitting the correct solution - this can also happen :)")
    }

  }


  /**
    * checking the parity of a 0/1 sequence
    */
  def parityExample(): Unit = {
    val random = new Random()

    val seqLen = 8
    val zos = new ZeroOneSequence(seqLen)

    //how many teaching iterations do you want?
    val iterationCount = 600

    //every how many iterations do you want to see the debug output?
    val statsPeriod = 10

    /*
     * you can include multiple configurations to see how they compare, just edit the lists
     */
    // how many neurons should there be in the hidden layer?
    val hiddenLayerSizes = List(20)
    val betas = List(2.0)
    val gammas = List(0.16)

    // do you want to use the bias neurons?
    val biases = List(true)

    /*
     *  if you want to run the whole test scenario multiple times, change the passCount
     *
     *  initially the weights are initialized randomly, so some passes might get good results
     *  sooner than others
     */
    val passCount = 1

    val total_passes = hiddenLayerSizes.length * betas.length * gammas.length * biases.length * passCount
    println("total pass count: " + total_passes)

    for (
      hiddenSize <- hiddenLayerSizes;
      beta <- betas;
      gamma <- gammas;
      bias <- biases
    ) {
      println( "hidden layers: %d, beta: %f gamma: %f, bias: %B" format(hiddenSize, beta, gamma, bias))
      Range(0, passCount).foreach{ pass =>
        println("pass: " + pass)
        val nn: NeuralNetwork = new FeedForwardNeuralNetwork(List(seqLen, hiddenSize, 1), new SigmoidFunction(beta), gamma, bias)

        Range(0, iterationCount).inclusive.foreach{ iteration =>
          /*
           * in each iteration we get all the possible sequences, shuffle them, and feed them to the
           * neural network along with the correct parity of the sequence
           */
          val teachingExamples = random.shuffle(zos.allSequences)
          teachingExamples.foreach { teachingSequence =>
            nn.trainInt(teachingSequence, Seq(ZeroOneSequence.checkParity(teachingSequence)))
          }
          if(iteration % statsPeriod == 0) {
            var successCount = 0;
            zos.allSequences.foreach { seq =>
              if(ZeroOneSequence.checkParity(seq) == nn.classifyInt(seq).head)
                successCount += 1
            }
            val successRate: Double = 1.0 * successCount / zos.combinationCount
            println("success rate after %d iterations: %f" format (iteration, successRate))
          }
        }
      }
    }
  }
}