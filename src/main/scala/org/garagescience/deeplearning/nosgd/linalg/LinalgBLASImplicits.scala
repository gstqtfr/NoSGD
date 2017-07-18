package org.garagescience.deeplearning.nosgd.linalg

import org.jblas.DoubleMatrix
import scala.language.implicitConversions

object LinalgBLASImplicits {

  def rowAsArray(m: DoubleMatrix)(i: Int): Array[Double] =
    m.getRow(i).toArray

  implicit def doubleBLASMatrix2LinalgMatrix(m: DoubleMatrix): Matrix[Double] = {

    val builder = MatrixBuilder[Double]() at 0
    val data = (0 until m.getRows) map { i =>
      rowAsArray(m)(i)
    }
    val vrep = (0 until m.getRows) map {i =>
      builder += Vector.at(0)(data(i): _*)
    }
    builder.result()
  }

  implicit def linalgMatrix2DoubleBLASMatrix(m: Matrix[Double]): DoubleMatrix = {
    val jblas = DoubleMatrix.zeros(m.height, m.width)
    (0 until m.height) map {r =>
      (0 until m.width) foreach {c =>
        jblas.put(r, c, m(r,c))
      }

    }
    jblas
  }

  /*
  implicit def doubleBLASMatrix2Array(m: DoubleMatrix): Array[Double] ={
    (0 until m.getRows) flatMap { i =>
      rowAsArray(m)(i)
    }
  }.toArray
  */

}

