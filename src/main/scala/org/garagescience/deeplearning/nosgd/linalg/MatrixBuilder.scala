/*
  LinAlg - Scala Library for Vector and Matrix Types and Operations

  Copyright 2015-2016 Hans-Hermann Bode

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/

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
