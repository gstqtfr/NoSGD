package org.garagescience.deeplearning.nosgd

object SimpleDoubleEG1 {

  import SingleShotMutation._

  def distance(d: Double): Double = Math.abs(d - 0.0)

  def main(args: Array[String]): Unit = {

    val randomSeq = (0 to 20).map { i => scala.util.Random.nextGaussian * 100.0}

    val result: scala.collection.immutable.Seq[Double] = germinate(randomSeq)(distance)(100)

    println("random seq:")
    for (seq <- randomSeq)
      println(s"$seq")

    println("result seq:")
    for (seq <- result)
      println(s"$seq")

  }

}
