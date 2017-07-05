package org.garagescience.deeplearning.nosgd.linalg

import org.garagescience.deeplearning.nosgd.linalg.Vector.At

import scala.collection.immutable.IndexedSeq

object LinalgUtils {

  def randomSeq(n: Int): Seq[Double] = Seq.fill(n)(scala.util.Random.nextGaussian)


  def randomVector(n: Int): Vector[Double] = Vector.fromSeq({
    randomSeq(n)
  })(At(0))


  def randomMatrix(height: Int, width: Int, sz: Int = 9): Matrix[Double] = {

    // generate a bunch of random Vectors
    val vxs: IndexedSeq[Vector[Double]] = for {row <- 0 until height} yield randomVector(width)

    val builder = MatrixBuilder[Double]()
    for (row <- 0 until height)
      builder += vxs(row)

    builder.result()
  }

  def zeroVector(n: Int): Vector[Double] = Vector({Seq.fill(n)(0.0): _*})

  //def zeroVector(n: Int): Vector[Double] = Vector.fromSeq({Seq.fill(n)(0.0): _*})(At(0))

  def zeroMatrix(height: Int, width: Int, sz: Int = 9): Matrix[Double] = {

    // generate a bunch of random Vectors
    val vxs: IndexedSeq[Vector[Double]] = for {row <- 0 until height} yield zeroVector(width)

    val builder = MatrixBuilder[Double]()
    for (row <- 0 until height)
      builder += vxs(row)

    builder.result()
  }


}