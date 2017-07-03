package org.garagescience.deeplearning.nosgd.linalg

import breeze.linalg.{CSCMatrix => BSM, DenseMatrix => BDM, Matrix => BM}
import org.apache.spark.ml.{linalg => mllinalg}



/**
  * Implicit methods available in Scala for converting [[org.garagescience.deeplearning.nosgd.linalg._Matrix]] to
  * [[org.apache.spark.ml.linalg.Matrix]] and [[breeze.linalg.DenseMatrix]] & vice versa.
  */

object _MatrixImplicits {


  implicit def _DenseMatrixToMLDenseMatrix(m: _DenseMatrix): mllinalg.DenseMatrix = m.asML

  implicit def _DenseMatrixToBreezeDenseMatrix(m: _DenseMatrix): BM[Double] = m.asBreeze

  // TODO: add to's as well as from's

  implicit def breezeDenseMatrixTo_DenseMatrix(m: BDM[Double]): _DenseMatrix =
    _Matrix.fromBreeze(m)

  implicit def mlDenseMatrixTo_DenseMatrix(m: mllinalg.DenseMatrix): _DenseMatrix =
    _Matrix.fromML(m)


}