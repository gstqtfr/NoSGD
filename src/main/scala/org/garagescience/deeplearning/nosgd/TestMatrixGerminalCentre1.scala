package org.garagescience.deeplearning.nosgd

import breeze.numerics.abs
import org.apache.spark.ml.linalg.{Matrices, Matrix => MMatrix}
import org.garagescience.deeplearning.nosgd.linalg.Vector.At

import scala.collection.immutable.{IndexedSeq, Seq}
import org.garagescience.deeplearning.nosgd.linalg._

object TestMatrixGerminalCentre1 {

  private final val iterations = 500
  private final val popSize = 50
  private final val poolSize = 20
  // TODO: this makes usre we start at 0 - let's make this the default!!!
  private implicit val Low = At(0)

  // this is actually a Really Hard problem! ...

  // this is a test of the matrix builder
  val builder = MatrixBuilder[Double]()
  val v1 = Vector(0.0, 0.0, 0.0)
  val v2 = Vector(0.0, 0.0, 0.0)
  val v3 = Vector(0.0, 0.0, 0.0)
  builder += v1
  builder += v2
  builder += v3
  private final val target = builder.result()

  private def randomSeq(n: Int): Seq[Double] = Seq.fill(n)(scala.util.Random.nextGaussian)

  // TODO: investigate: do we actually need the flatten here?
  private def randomVector(n: Int): Vector[Double] = Vector.fromSeq({
    randomSeq(n)
  })(At(0))

  // build a matrix with random weights from a Gaussian dist.
  private def randomMatrix(height: Int, width: Int, sz: Int = 9): Matrix[Double] = {

    // generate a bunch of random Vectors
    val vxs: IndexedSeq[Vector[Double]] = for {row <- 0 until height} yield randomVector(width)

    val builder = MatrixBuilder[Double]()
    for (row <- 0 until height)
      builder += vxs(row)

    builder.result()
  }

  // we always need to remember to add atRow(0):
  // val b = Matrix.atRow(0)(Vector(11,12,13), Vector(21,22,23))
  // so we get the indices right ...


  // TODO: okay, we have the 1st one, but let's try another few ...

  private def error1(m1: Matrix[Double]): Double = (m1 - target).rowSum().sum

  private def error(m1: Matrix[Double]): Double =
    Math.sqrt(
      Math.pow(Math.abs((for {i <- 0 until target.height
                              j <- 0 until target.width}
        yield target(i, j) - m1(i, j)).sum), 2.0)
    )

  // (target - m1).abs.toArray.sum

  private def getRMSE(xs: Array[Double]): Double =
    Math.sqrt(Math.pow(Math.abs(xs.sum / popSize), 2.0))

  def main(args: Array[String]): Unit = {

    val mgc: IndexedSeq[LinalgMatrixGerminalCentre] = for {i <- 0 until popSize}
      yield new LinalgMatrixGerminalCentre(randomMatrix(
        target.height,
        target.width
      ), poolSize)

    for (i <- 0 until iterations) {

      mgc.map(gc => gc.update(error))

      val errors = mgc.map(gc => gc.clones).
        map(xs => xs.map(d => error(d)))

      errors.foreach(xs => println(s"$i ${getRMSE(xs)}"))
      println()
    }

    // this is more to do with having something to slap on the screen
    // than actually necessary!
    val cloneattack: IndexedSeq[Matrix[Double]] = mgc.map(gc => gc.clones).
      flatMap(xsm => xsm)


    cloneattack.foreach { xs =>
      xs.foreach(m => println(s"$m\n"))
      println()
    }

  }

}