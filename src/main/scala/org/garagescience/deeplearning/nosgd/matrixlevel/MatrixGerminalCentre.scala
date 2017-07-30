package org.garagescience.deeplearning.nosgd.matrixlevel

import org.garagescience.deeplearning.nosgd.bca.Double2BitStringConvert._
import org.garagescience.deeplearning.nosgd.bca.GerminalCentre
import org.jblas.DoubleMatrix

import scala.collection.immutable.IndexedSeq


// TODO: we're doing this from the top, clean room design
class MatrixGerminalCentre(val m: DoubleMatrix, val poolSize: Int = 20) {

  protected val r = new scala.util.Random

  protected def index(idx: Int, length: Int): Int = idx % length

  protected def flipbit: Char = if (r.nextDouble >= 0.5) '1' else '0'

  var clones: Array[DoubleMatrix] = (for {i <- 0 until poolSize} yield m).toArray

  // so, we need to Do Stuff here so that we get all the doubles out
  // of the matrix, mutate them, ADD THEM to the original values!

  // need to iterate over the whole matrix, get(row,col)
  def germinate: Array[IndexedSeq[IndexedSeq[DoubleMatrix]]] = clones.
    map { matrix =>
      (0 until matrix.rows).map { row =>
        (0 until matrix.columns).map { col =>
          matrix.put(row, col,
            somaticHypermutation(new StringBuffer(toBinaryString(matrix.get(row, col)))))
        }
      }
    }


  // so, we need to deconstruct/reconstruct the matrix

  def somaticHypermutation(_sb: StringBuffer): Double = {

    val length = scala.util.Random.nextInt(_sb.length)

    def _somaticHypermutation(sb: StringBuffer): Double = {

      // this is biased to the LSB, so lots of activity in the
      // mantissa ...
      for (i <- 0 until length)
        sb.setCharAt(index(i, sb.length), flipbit)

      fromBinaryString(sb.reverse.toString)
    }

    val __sb = _sb.reverse
    var _d: Double = 0

    do {
      _d = _somaticHypermutation(__sb)
    } while (java.lang.Double.isNaN(_d) || java.lang.Double.isInfinite(_d))

    _d
  }

}
