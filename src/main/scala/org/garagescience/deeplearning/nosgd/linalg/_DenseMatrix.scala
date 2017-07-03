package org.garagescience.deeplearning.nosgd.linalg

import breeze.linalg.{CSCMatrix => BSM, DenseMatrix => BDM, Matrix => BM}
import org.apache.spark.ml.{linalg => mllinalg}

import scala.reflect.ClassTag

/*

// testing function for _DenseMatrix

def randomMatrix(rows: Int, cols: Int, sz: Int) = {
    val tmpArray: Array[Double] = (for {i <- 0 until sz} yield scala.util.Random.nextGaussian).toArray
    new _DenseMatrix(rows, cols, tmpArray, false)
}

*/

class _DenseMatrix[T](val numRows: Int,
                   val numCols: Int,
                   val values: Array[T],
                   override val isTransposed: Boolean) extends _Matrix[T] {

  require(values.length == numRows * numCols, "The number of values supplied doesn't match the " +
    s"size of the matrix! values.length: ${values.length}, numRows * numCols: ${numRows * numCols}")

  /**
    * Column-major dense matrix.
    * The entry values are stored in a single array of doubles with columns listed in sequence.
    * For example, the following matrix
    * {{{
    *   1.0 2.0
    *   3.0 4.0
    *   5.0 6.0
    * }}}
    * is stored as `[1.0, 3.0, 5.0, 2.0, 4.0, 6.0]`.
    *
    * @param numRows number of rows
    * @param numCols number of columns
    * @param values  matrix entries in column major
    */

  def this(numRows: Int, numCols: Int, values: Array[T]) =
    this(numRows, numCols, values, false)

  // TODO: hmm ... code smell here ...
  override def equals(o: Any): Boolean = o match {
    case m: _Matrix[T] => asBreeze == m.asBreeze
    case _ => false
  }

  /*
  override def hashCode: Int = {
    com.google.common.base.Objects.hashCode(numRows: Integer, numCols: Integer, toArray)
  }
  */

  def apply(i: Int): T = values(i)

  override def apply(i: Int, j: Int): T = values(index(i, j))

  def index(i: Int, j: Int): Int = {
    require(i >= 0 && i < numRows, s"Expected 0 <= i < $numRows, got i = $i.")
    require(j >= 0 && j < numCols, s"Expected 0 <= j < $numCols, got j = $j.")
    if (!isTransposed) i + numRows * j else j + numCols * i
  }

  def update(i: Int, j: Int, v: T): Unit = {
    values(index(i, j)) = v
  }

  override def copy: _DenseMatrix[T] = new _DenseMatrix(numRows, numCols, values.clone())

  override def map(f: T => T)(implicit c: ClassTag[T]): _DenseMatrix[T] = {
    val ar: Array[T] = values map f
    new _DenseMatrix(numRows, numCols, ar, isTransposed)
  }

  def update(f: T => T): _DenseMatrix[T] = {
    val len = values.length
    var i = 0
    while (i < len) {
      values(i) = f(values(i))
      i += 1
    }
    this
  }

  override def transpose: _DenseMatrix[T] = new _DenseMatrix(numCols, numRows, values, !isTransposed)

  override def foreach(f: (Int, Int, T) => Unit): Unit = {
    if (!isTransposed) {
      // outer loop over columns
      var j = 0
      while (j < numCols) {
        var i = 0
        val indStart = j * numRows
        while (i < numRows) {
          f(i, j, values(indStart + i))
          i += 1
        }
        j += 1
      }
    } else {
      // outer loop over rows
      var i = 0
      while (i < numRows) {
        var j = 0
        val indStart = i * numCols
        while (j < numCols) {
          f(i, j, values(indStart + j))
          j += 1
        }
        i += 1
      }
    }
  }


  def asBreeze: BM[T] = {
    if (!isTransposed) {
      new BDM[T](numRows, numCols, values)
    } else {
      val breezeMatrix = new BDM[T](numCols, numRows, values)
      breezeMatrix.t
    }
  }




  // TODO: make this more general by applying an op

  /*
  def -(m: BDM[T])(implicit c: ClassTag[T]): _DenseMatrix[T] = {
    require(m.rows == this.numRows, s"rows mismatch: ${m.rows} != ${numRows}")
    require(m.cols == this.numCols, s"columns mismatch: ${m.cols} != ${numCols}")

    val me: BDM[T] = this.asBreeze.toDenseMatrix
    val result: BDM[T] = m - me

    for {i <- 0 until this.numRows
        j <- 0 until this.numCols}

    new _DenseMatrix(this.numRows, this.numCols, result.data, this.isTransposed)
  }
  */

  // TODO: need to sort this - we need operators like +, -, u.s.w. ...

/*
  def -(m: _DenseMatrix[T]): _DenseMatrix[T] = {
    require(m.numRows == this.numRows, s"rows mismatch: ${m.numRows} != ${numRows}")
    require(m.numCols == this.numCols, s"columns mismatch: ${m.numCols} != ${numCols}")

    // this.values - m.values

    val data: Array[T] = (for {i <- 0 until m.numRows
                                    j <- 0 until m.numCols} yield m.apply(i,j) - this.apply(i,j)).toArray


    new _DenseMatrix[T](this.numRows, this.numCols, data, this.isTransposed)
  }
*/

  //def abs: _DenseMatrix[T] =
  //  new _DenseMatrix(numRows, numCols, values map { e => if (e < 0) -e else e }, isTransposed)

}
