package org.garagescience.deeplearning.nosgd

import breeze.numerics.abs
import org.apache.spark.ml.linalg.{Matrices, Matrix}

import scala.collection.immutable.Seq
import org.garagescience.deeplearning.nosgd.linalg._

// TODO: Future. or Actor. get it sorted!!!

object TestMatrixGerminalCentre1 {

  import org.garagescience.deeplearning.nosgd.mlp.MathUtils._

  private final val iterations = 500
  private final val popSize = 50
  private final val poolSize = 20

  // this is actually a Really Hard problem! ...

  private final val target = new _DenseMatrix(3, 3,
    Array(
      0.0, 0.0, 0.0,
      0.0, 0.0, 0.0,
      0.0, 0.0, 0.0
    )
  )

  private def randomMatrix(rows: Int, cols: Int, sz: Int): _DenseMatrix[Double] = {
    val tmpArray = for {i <- 0 until sz} yield scala.util.Random.nextGaussian
    new _DenseMatrix(rows, cols, tmpArray.toArray)
  }

  // TODO: implement a matrix distance, e.g.:
  // d1(A,B)=∑i=1n∑j=1n|aij−bij|

  // TODO: okay, we have the 1st one, but let's try another few ...

  //private def error1(m1: _DenseMatrix): Double = (m1 - target).toArray.sum

  private def error1(m1: _DenseMatrix[Double]): Double = {
    //val tmp1: _Matrix = m1 - target

    0.0
  }

  private def error(m1: _Matrix[Double]): Double =
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
      yield new LinalgMatrixGerminalCentre(randomMatrix(
        target.numRows,
        target.numCols,
        target.toArray.length
      ), poolSize)

    for (i <- 0 until iterations) {


      mgc.map(gc => gc.update(error))

      val cloneattack: Seq[Seq[_Matrix[Double]]] = mgc.map(gc => gc.clones)

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