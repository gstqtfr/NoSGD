package org.garagescience.deeplearning.nosgd.linalg

import breeze.linalg.{CSCMatrix => BSM, DenseMatrix => BDM, Matrix => BM}
import org.apache.spark.ml.linalg.{Matrices,DenseMatrix}
import scala.reflect._
import scala.reflect.runtime.universe._

// i needed to create a typeclass _Matrix & get implicits to convert between breeze
// linalg matrices & spark ml matrices & _Matrix

// this gives us a general type to aim for; it also allows us to easily interact with
// spark ml & breeze linalg

// make this private[linalg]?
// TODO: type-param this?!?
trait _Matrix[T] extends Serializable {

  /** Number of rows. */
  def numRows: Int

  /** Number of columns. */
  def numCols: Int

  //  TODO: when this is type-param'd, we can do this:
  def values: Array[T]

  // TODO: do we need this?
  /** Flag that keeps track whether the matrix is transposed or not. False by default. */
  val isTransposed: Boolean = false

  /** Converts to a dense array in column major. */
  def toArray(implicit c: ClassTag[T]): Array[T] = {
    val newArray = new Array[T](numRows * numCols)
    foreach { (i, j, v) =>
      newArray(j * numRows + i) = v
    }
    newArray
  }

  // TODO: we may need to get rid of this ...
  // def -(m: BDM[T]): _Matrix[T]

  //def -(m: _Matrix): _Matrix

  /** Converts to a breeze matrix. */
  def asBreeze: BM[T]

  /** Gets the (i, j)-th element. */
  def apply(i: Int, j: Int): T

  /** Return the index for the (i, j)-th element in the backing array. */
  def index(i: Int, j: Int): Int

  /** Update element at (i, j) */
  def update(i: Int, j: Int, v: T): Unit

  /** Get a deep copy of the matrix. */
  def copy: _Matrix[T]

  /**
    * Transpose the Matrix. Returns a new `_Matrix` instance sharing the same underlying data.
    */
  def transpose: _Matrix[T]


  /** A human readable representation of the _Matrix */
  override def toString: String = asBreeze.toString()

  /**
    * Map the values of this matrix using a function. Generates a new matrix. Performs the
    * function on only the backing array. For example, an operation such as addition or
    * subtraction will only be performed on the non-zero values in a `SparseMatrix`.
    */
  def map(f: T => T)(implicit c: ClassTag[T]): _Matrix[T]

  /**
    * Update all the values of this matrix using the function f. Performed in-place on the
    * backing array. For example, an operation such as addition or subtraction will only be
    * performed on the non-zero values in a `SparseMatrix`.
    */
  def update(f: T => T): _Matrix[T]

  /**
    * Applies a function `f` to all the active elements of dense and sparse matrix. The ordering
    * of the elements are not defined.
    *
    * @param f the function takes three parameters where the first two parameters are the row
    *          and column indices respectively with the type `Int`, and the final parameter is the
    *          corresponding value in the matrix with type `Double`.
    */
  def foreach(f: (Int, Int, T) => Unit)

}

object _Matrix {

  def fromBreeze[T](breeze: BM[T]): _DenseMatrix[T] = {
    breeze match {
      case dm: BDM[T] =>
        new _DenseMatrix[T](dm.rows, dm.cols, dm.data, dm.isTranspose)
      case _ =>
        throw new UnsupportedOperationException(
          s"Do not support conversion from type ${breeze.getClass.getName}.")
    }
  }

  // TODO: had to specialise this - DOUBLE PLUS UNGOOD!!!
  // TODO: GET THIS SORTED!!!
  def fromML(m: DenseMatrix): _DenseMatrix[Double] =
    new _DenseMatrix(m.numRows, m.numCols, m.values, m.isTransposed)



}