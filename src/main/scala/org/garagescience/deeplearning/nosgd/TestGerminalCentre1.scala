package org.garagescience.deeplearning.nosgd

import scala.collection.immutable.Seq

object TestGerminalCentre1 {

  private final val iterations = 50
  private final val popSize = 10
  private final val target = 0.0

  def error(d: Double) = Math.abs(target - d)

  def getRMSE(xs: Seq[Double]): Double = xs.sum / popSize

  def main(args: Array[String]): Unit = {

    val gcs: Seq[DoubleGerminalCentre] =
      for {i <- 0 until popSize}
        yield new DoubleGerminalCentre(scala.util.Random.nextGaussian, 20)

    for (i <- 0 until iterations) {

      gcs.map(gc => gc.update(error))
      val cloneattack: Seq[Seq[Double]] = gcs.map(gc => gc.clones)
      val errors: Seq[Seq[Double]] = gcs.map(gc => gc.clones).
        map(xs => xs.map(d => error(d)))

      errors.foreach(xs => println(s"$i ${getRMSE(xs)}"))
      println()
    }

  }

}