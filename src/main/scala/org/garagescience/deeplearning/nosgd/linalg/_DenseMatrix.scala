package org.garagescience.deeplearning.nosgd.linalg

import breeze.linalg.{CSCMatrix => BSM, DenseMatrix => BDM, Matrix => BM}

class _DenseMatrix(val numRows: Int,
                   val numCols: Int,
                   val values: Array[Double],
                   override val isTransposed: Boolean) extends _Matrix {

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

  def this(numRows: Int, numCols: Int, values: Array[Double]) =
    this(numRows, numCols, values, false)

  override def equals(o: Any): Boolean = o match {
    case m: _Matrix => asBreeze == m.asBreeze
    case _ => false
  }

  override def hashCode: Int = {
    com.google.common.base.Objects.hashCode(numRows: Integer, numCols: Integer, toArray)
  }

  def asBreeze: BM[Double] = {
    if (!isTransposed) {
      new BDM[Double](numRows, numCols, values)
    } else {
      val breezeMatrix = new BDM[Double](numCols, numRows, values)
      breezeMatrix.t
    }
  }

  def apply(i: Int): Double = values(i)

  override def apply(i: Int, j: Int): Double = values(index(i, j))

  def index(i: Int, j: Int): Int = {
    require(i >= 0 && i < numRows, s"Expected 0 <= i < $numRows, got i = $i.")
    require(j >= 0 && j < numCols, s"Expected 0 <= j < $numCols, got j = $j.")
    if (!isTransposed) i + numRows * j else j + numCols * i
  }

  def update(i: Int, j: Int, v: Double): Unit = {
    values(index(i, j)) = v
  }

  override def copy: _DenseMatrix = new _DenseMatrix(numRows, numCols, values.clone())

  def map(f: Double => Double) = new _DenseMatrix(numRows, numCols, values.map(f),
    isTransposed)

  def update(f: Double => Double): _DenseMatrix = {
    val len = values.length
    var i = 0
    while (i < len) {
      values(i) = f(values(i))
      i += 1
    }
    this
  }

  override def transpose: _DenseMatrix = new _DenseMatrix(numCols, numRows, values, !isTransposed)

  override def foreach(f: (Int, Int, Double) => Unit): Unit = {
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

  //override def numNonzeros: Int = values.count(_ != 0)

  //override def numActives: Int = values.length

  /**
    * Generate a `SparseMatrix` from the given `_DenseMatrix`. The new matrix will have isTransposed
    * set to false.
    */
  /*
  def toSparse: SparseMatrix = {
    val spVals: MArrayBuilder[Double] = new MArrayBuilder.ofDouble
    val colPtrs: Array[Int] = new Array[Int](numCols + 1)
    val rowIndices: MArrayBuilder[Int] = new MArrayBuilder.ofInt
    var nnz = 0
    var j = 0
    while (j < numCols) {
      var i = 0
      while (i < numRows) {
        val v = values(index(i, j))
        if (v != 0.0) {
          rowIndices += i
          spVals += v
          nnz += 1
        }
        i += 1
      }
      j += 1
      colPtrs(j) = nnz
    }
    new SparseMatrix(numRows, numCols, colPtrs, rowIndices.result(), spVals.result())
  }
  */

  /*
  override def colIter: Iterator[Vector] = {
    if (isTransposed) {
      Iterator.tabulate(numCols) { j =>
        val col = new Array[Double](numRows)
        blas.dcopy(numRows, values, j, numCols, col, 0, 1)
        new DenseVector(col)
      }
    } else {
      Iterator.tabulate(numCols) { j =>
        new DenseVector(values.slice(j * numRows, (j + 1) * numRows))
      }
    }
  }
  */


  /*
  override def asML: newlinalg._DenseMatrix = {
    new newlinalg._DenseMatrix(numRows, numCols, values, isTransposed)
  }
  */
}
