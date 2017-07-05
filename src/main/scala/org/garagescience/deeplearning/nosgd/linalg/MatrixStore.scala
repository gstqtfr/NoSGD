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

import scala.reflect.ClassTag

/**
 * A storage for matrix elements.
 *
 * @param <E> type of the elements
 * @since 2.0.0
 * @author h2b
 */
trait MatrixStore [E] {

  protected implicit val elemTag: ClassTag[E]

  val index: Index2

  /**
   * @param i the row index
   * @return the row vector with index {@code i}
   */
  def row (i: Int): Vector[E]

  /**
   * @param j the column index
   * @return the column vector with index {@code j}
   */
  def col (j: Int): Vector[E]

  /**
   * @param i the row index
   * @return the row vector with index {@code i}
   */
  def apply (i: Int): Vector[E] = row(i)

  /**
   * @param i the row index
   * @param j the column index
   * @return the element with index {@code i,j}
   */
  def apply (i: Int, j: Int): E

  protected val dataHashCode: Int

  override def hashCode = {
    val prime = 31
    var result = 1
    result = prime*result+index.dim1.low
    result = prime*result+index.dim1.high
    result = prime*result+index.dim2.low
    result = prime*result+index.dim2.high
    result = prime*result+dataHashCode
    result
  }

}

/**
 * Implementation trait that uses `scala.collection.immutable.Vector` as storage
 * of row vectors.
 *
 * @param <E> type of the elements
 * @since 2.0.0
 * @author h2b
 */
trait RowMatrixStore [E] extends MatrixStore[E] {

  protected val rowStart: Int
  protected val rows: Seq[Vector[E]]

  private val data = scala.collection.immutable.Vector(rows: _*)

  protected val dataHashCode: Int = data.hashCode

  private val rowIndex = Index(rowStart, rowStart+data.length-1)

  private val colIndex = {
    var min = Index.Maxdex
    var max = Index.Mindex
    for (v <- data)
      if (v.index.low<min) min = v.index.low
      else if (v.index.high>max) max = v.index.high
    Index(min, max)
  }

  val index = Index2(rowIndex, colIndex)

  private val zerovec = Vector.at[E](colIndex.low)()

  def row (i: Int): Vector[E] =
    if (rowIndex.contains(i)) data(i-rowStart) else zerovec

  def col (j: Int): Vector[E] =
    Vector((i: Int) => data(i-rowStart)(j), rowIndex.low, rowIndex.high)

  def apply (i: Int, j: Int): E = row(i)(j)

}
