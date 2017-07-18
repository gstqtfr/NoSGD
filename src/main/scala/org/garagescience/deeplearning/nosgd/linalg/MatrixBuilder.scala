package org.garagescience.deeplearning.nosgd.linalg

import scala.collection.mutable.{ ArrayBuffer, Builder }
import scala.reflect.ClassTag

/**
 * Base trait of matrix builders.
 *
 * @param <V> the type of the row or column vectors
 * @param <M> the type of resulting matrix
 * @author h2b
 */
trait MatrixBuilder[V, +M] extends Builder[V, M] {

  protected var rowStart = 1;

  protected val elems = ArrayBuffer.empty[V]

  /**
   * Used to fill empty spots when widening the index range.
   */
  protected val zerovec: V

  def at (row: Int): this.type = { rowStart = row; this }

  def += (x: V): this.type = { elems += x; this }

  def clear () = elems.clear()

  def result (): M

  /**
   * Sets the element with the specified index to the given value.
   * Widens the concrete index range if necessary,
   *
   * @param i the index
   * @param x the value
   */
  def update (i: Int, x: V): Unit = {
    val elemsRow = i-rowStart
    if (elemsRow<0) {
      rowStart = i
      ArrayBuffer.fill(-elemsRow-1)(zerovec) ++=: elems
      x +=: elems
    } else if (elemsRow>=elems.length) {
      elems ++= ArrayBuffer.fill(elemsRow-elems.length)(zerovec)
      elems += x
    } else {
      elems(elemsRow) = x
    }
  }

}

object MatrixBuilder {

	/**
	 * @return a default matrix builder
	 */
	def apply [E : ClassTag] (): MatrixBuilder[Vector[E], Matrix[E]] = new DefaultMatrixBuilder[E]

}

class DefaultMatrixBuilder [E : ClassTag] private[linalg] extends MatrixBuilder[Vector[E], Matrix[E]] {

  protected val zerovec = Vector[E]()

  def result () = MatrixFactory(rowStart, elems)

}
