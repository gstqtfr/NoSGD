package org.garagescience.deeplearning.nosgd

import org.apache.spark.ml.linalg.{Matrices, Matrix}
import org.apache.spark.ml.linalg.{Vectors, Vector}
import scala.collection.immutable.{Seq => ISeq}

object Matrix2BinarySeq {

  implicit class M2BSeq(val m: Matrix) {

    def toSeqOfSeq: ISeq[ISeq[Double]] = {

      def getRow(row: Int): ISeq[Double] =
        for {i <- 0 until m.numCols}
          yield m(row, i)

      for {row <- 0 until m.numCols} yield getRow(row)
    }

    // TODO: move the implicit to Matrices instead of Matrix?
    def fromSeqOfSeq(sos: ISeq[ISeq[Double]]): Matrix =
      Matrices.dense(
        sos.length,
        sos(0).length,
        { sos.flatMap(xs => xs).toArray } ).transpose
  }
}

object Vector2BinarySeq {

  implicit class V2BSeq(val v: Vector) {

    def toSeq: ISeq[Double] = ISeq[Double](v.toArray: _*)

    // def fromSeq(xs: Seq[Double]) = Vector[Double](xs: _*)

    def fromSeq(xs: ISeq[Double]): Vector = Vectors.dense(xs.toArray)
  }
}


