package org.garagescience.deeplearning.nosgd

import org.apache.spark.ml.linalg.{Vector, Vectors}
import scala.collection.immutable.Seq

// TODO: repeat for Matrix. just treat it as a Seq[Seq[Double]]
// TODO: this is GOING TO BE EASY!!!

object TestVectorGerminalCentre1 {

  import org.garagescience.deeplearning.nosgd.mlp.MathUtils._

  private final val iterations = 100
  private final val popSize = 10

  private final val target = Vectors.dense(Array(0.0, 0.0, 0.0))
  private final val targetSize = target.size

  private def randomVector(sz: Int) = {
    val tmpArray = for {i <- 0 until sz} yield scala.util.Random.nextGaussian
    Vectors.dense(tmpArray.toArray)
  }

  // could partially apply this, bind it to target ...
  private def error(v1: Vector): Double =  Math.pow(Math.sqrt((target - v1).abs.toArray.sum), 2.0)

  // TODO: & try it here, too! this is NOT RMSE! FFS! TLA!!!
  private def getRMSE(xs: Seq[Double]): Double = xs.sum / popSize


  def main(args: Array[String]): Unit = {

    val vgc: Seq[VectorGerminalCentre] =
      for {i <- 0 until popSize}
        yield new VectorGerminalCentre(randomVector(targetSize), 50)

    for (i <- 0 until iterations) {
      vgc.map(gc => gc.update(error))


      val errors: Seq[Seq[Double]] = vgc.map(gc => gc.clones).
        map(xs => xs.map(d => error(d)))

      errors.foreach(xs => println(s"$i ${getRMSE(xs)}"))
      println()

    }


  }

}