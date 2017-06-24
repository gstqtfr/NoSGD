package org.garagescience.deeplearning.nosgd

import org.apache.spark.ml.linalg.{Matrix, Matrices}
import scala.collection.immutable.Seq

// TODO: Future. or Actor. get it sorted!!!

object TestMatrixGerminalCentre1 {

  import org.garagescience.deeplearning.nosgd.mlp.MathUtils._

  private final val iterations = 500
  private final val popSize = 50
  private final val poolSize = 20

  // this is actually a Really Hard problem! ...

  private final val target = Matrices.dense(3, 3,
    Array(
      0.0, 0.0, 0.0,
      0.0, 0.0, 0.0,
      0.0, 0.0, 0.0
    )
  )

  private def randomMatrix(rows: Int, cols: Int, sz: Int) = {
    val tmpArray = for {i <- 0 until sz} yield scala.util.Random.nextGaussian
    Matrices.dense(rows, cols, tmpArray.toArray)
  }

  // TODO: implement a matrix distance, e.g.:
  // d1(A,B)=∑i=1n∑j=1n|aij−bij|

  // TODO: okay, we have the 1st one, but let's try another few ...

  private def error1(m1: Matrix): Double = (target - m1).abs.toArray.sum

  private def error(m1: Matrix): Double =
    Math.sqrt(
      Math.pow(Math.abs((for {i <- 0 until target.numRows
        j <- 0 until target.numCols}
        yield target(i,j) - m1(i,j)).sum), 2.0)
    )

    // (target - m1).abs.toArray.sum

  private def getRMSE(xs: Seq[Double]): Double =
    Math.sqrt(Math.pow(Math.abs(xs.sum / popSize), 2.0))

  def main(args: Array[String]): Unit = {

    val mgc = for {i <- 0 until popSize}
      yield new MatrixGerminalCentre(randomMatrix(
        target.numRows,
        target.numCols,
        target.toArray.length
      ), poolSize)

    for (i <- 0 until iterations) {

      // TODO: add a promise/future call here ...

      mgc.map(gc => gc.update(error))

      val cloneattack: Seq[Seq[Matrix]] = mgc.map(gc => gc.clones)

      val errors: Seq[Seq[Double]] = mgc.map(gc => gc.clones).
        map(xs => xs.map(d => error(d)))

      errors.foreach(xs => println(s"$i ${getRMSE(xs)}"))
      println()
    }

    // this is more to do with having something to slap on the screen
    // than actually necessary!
    val cloneattack: Seq[Seq[Matrix]] = mgc.map(gc => gc.clones).
      map(xsm => xsm.map(xs => Matrices.dense(target.numRows, target.numRows, xs.toArray)))

    cloneattack.foreach { xs =>
      xs.foreach(m => println(s"$m\n"))
      println()
    }




  }

}