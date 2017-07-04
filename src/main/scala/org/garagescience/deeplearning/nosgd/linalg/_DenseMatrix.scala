package org.garagescience.deeplearning.nosgd.linalg

import breeze.linalg.{CSCMatrix => BSM, DenseMatrix => BDM, Matrix => BM}
import org.apache.spark.ml.{linalg => mllinalg}
import scala.reflect.ClassTag
import org.garagescience.deeplearning.nosgd.linalg.LikeANumber.NumberLike
import scala.collection.JavaConverters

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

  /*
  def minus[T](m: _Matrix[T])(implicit numOps: NumberLike[T]): _DenseMatrix[T] = {
    // okay, i know, i hate asInstanceOf too, but sometimes you have to get your hands dirty ...
    val newArray: Array[T] = values.zip(m.values).map { case (a: T, b: T) => numOps.minus(a,b)}.asInstanceOf[Array[T]]
    new _DenseMatrix[T](m.numRows, m.numCols, newArray, isTransposed)
  }

  def plus[T](m: _Matrix[T])(implicit numOps: NumberLike[T]): _DenseMatrix[T] = {
    // okay, i know, i hate asInstanceOf too, but sometimes you have to get your hands dirty ...
    val newArray = values.zip(m.values).map { case (a: T, b: T) => numOps.plus(a,b)}.asInstanceOf[Array[T]]
    new _DenseMatrix[T](m.numRows, m.numCols, newArray, isTransposed)
  }

  def -[T](m: _Matrix[T])(implicit numOps: NumberLike[T]): _DenseMatrix[T] = minus(m)

  def +[T](m: _Matrix[T])(implicit numOps: NumberLike[T]): _DenseMatrix[T] = plus(m)

  def takeAway[T](c: T, m: _Matrix[T])(implicit numOps: NumberLike[T]): _DenseMatrix[T] = {
    val array: Array[T] = m.values
    // okay, i know, i hate asInstanceOf too, but sometimes you have to get your hands dirty ...
    val newArray: Array[T] = (array.map { e => numOps.minus(e,c) }).asInstanceOf[Array[T]]
    new _DenseMatrix[T](m.numRows, m.numCols, newArray, isTransposed)
  }
  */

}

object _DenseMatrix {

  def minus[T](m1: _DenseMatrix[T], m2: _DenseMatrix[T])(implicit numOps: NumberLike[T]): _DenseMatrix[T] = {
    require(m1.numRows == m2.numRows, s"Matrix rows don't match: ${m1.numRows} != ${m1.numRows}")
    require(m1.numCols == m2.numCols, s"Matrix rows don't match: ${m1.numCols} != ${m1.numCols}")

    val a1 = m1.values
    val a2 = m2.values

    val newValues: Array[T] = a1.zip(a2).map {
      case (a, b) => numOps.minus(a,b)
    }.asInstanceOf[Array[T]]

    //m1.values.zip(m2.values).map(NumOps.minus(_,_))

    new _DenseMatrix[T](m1.numRows, m1.numCols, newValues, m1.isTransposed)
  }

}
