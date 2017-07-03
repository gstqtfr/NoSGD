package org.garagescience.deeplearning.nosgd.linalg

import breeze.linalg.{CSCMatrix => BSM, DenseMatrix => BDM, Matrix => BM}
import org.apache.spark.ml.{linalg => mllinalg}


/**
  * Implicit methods available in Scala for converting [[org.garagescience.deeplearning.nosgd.linalg._Matrix]] to
  * [[org.apache.spark.ml.linalg.Matrix]] and [[breeze.linalg.DenseMatrix]] & vice versa.
  */

object _MatrixImplicits {


  //implicit def _DenseMatrixToMLDenseMatrix[T](m: _DenseMatrix[T]): mllinalg.DenseMatrix = m.asML

  implicit def _DenseMatrixToBreezeDenseMatrix[T](m: _DenseMatrix[T]): BM[T] = m.asBreeze

  // TODO: add to's as well as from's

  implicit def breezeDenseMatrixTo_DenseMatrix[T](m: BDM[T]): _DenseMatrix[T] =
    _Matrix.fromBreeze(m)

  // TODO: SPECIALISED!!! DOUBLE-PLUS UNGOOD!!! SORT IT OUT!!!!!
  /*
  implicit def mlDenseMatrixTo_DenseMatrix[Double](m: mllinalg.DenseMatrix): _DenseMatrix[Double] =
    _Matrix.fromML(m)
  */

}