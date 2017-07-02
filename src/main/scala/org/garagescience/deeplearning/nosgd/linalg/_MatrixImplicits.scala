package org.garagescience.deeplearning.nosgd.linalg


import breeze.linalg.{CSCMatrix => BSM, DenseMatrix => BDM, Matrix => BM}
import org.apache.spark.ml.linalg.DenseMatrix
import org.apache.spark.ml.{linalg => mllinalg}

/**
  * Implicit methods available in Scala for converting [[org.garagescience.deeplearning.nosgd.linalg._Matrix]] to
  * [[org.apache.spark.ml.linalg.Matrix]] and [[breeze.linalg.DenseMatrix]] & vice versa.
  */

object _MatrixImplicits {

  //implicit def mllibMatrixToMLMatrix(m: Matrix): newlinalg.Matrix = m.asML

  implicit def _DenseMatrixToMLDenseMatrix(m: _DenseMatrix): mllinalg.DenseMatrix = m.asML

  implicit def _DenseMatrixToBreezeDenseMatrix(m: _DenseMatrix): BM[Double] = m.asBreeze

  // TODO: add to's as well as from's

  //implicit def mllibSparseMatrixToMLSparseMatrix(m: SparseMatrix): newlinalg.SparseMatrix = m.asML

  // implicit def mlMatrixToMLlibMatrix(m: newlinalg.Matrix): Matrix = Matrices.fromML(m)

  // implicit def mlDenseMatrixToMLlibDenseMatrix(m: newlinalg.DenseMatrix): DenseMatrix =
    // Matrices.fromML(m).asInstanceOf[DenseMatrix]

  //implicit def mlSparseMatrixToMLlibSparseMatrix(m: newlinalg.SparseMatrix): SparseMatrix =
    // Matrices.fromML(m).asInstanceOf[SparseMatrix]
}