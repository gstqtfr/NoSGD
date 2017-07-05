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

import scala.collection.Iterator

/**
 * Defines immutable index ranges for matrices from {@code low} to {@code high}
 * (both inclusive) with step {@code 1} in two dimensions (rows and columns),
 * respectively.
 *
 * @author h2b
 */
class Index2 private (val dim1: Index, val dim2: Index) extends Seq[(Int, Int)] with Equals {

  override val isEmpty = dim1.isEmpty || dim2.isEmpty

  /**
   * @return the number of elements in this 2-dimensional index range
   */
  def longSize: Long = dim1.longSize*dim2.longSize

  /**
   * @return the number of elements in this 2-dimensional index range
   * @throws ArithmeticException if the size cannot be represented as {@code Int}
   */
  override def size: Int = {
    val l = longSize
    if (l<=Int.MaxValue) l.toInt
    else throw new ArithmeticException("integer overflow")
  }

  def length = size

  def apply (i: Int) = {
    if (i>=longSize) throw new IndexOutOfBoundsException(i.toString)
    (dim1.low+i/dim2.size, dim2.low+i%dim2.size)
  }

  /**
   * @return if (i, j) belongs to this 2-dimensional index range
   */
  def contains (i: Int, j: Int): Boolean = dim1.contains(i) && dim2.contains(j)

  /**
   * @return the intesection of this with that 2-dimensional index range
   * @note The result may be empty.
   */
  def intersect (that: Index2): Index2 =
    new Index2(this.dim1 intersect that.dim1, this.dim2 intersect that.dim2)

  /**
   * @return the union of this with that 2-dimensional index range
   * @note This is not a union of sets in mathematical sense since the result
   * will contain addtional elements if the original ranges are disjunct and not
   * adjacent.
   */
  def union (that: Index2): Index2 =
    new Index2(this.dim1 union that.dim1, this.dim2 union that.dim2)

  def iterator: Iterator[(Int, Int)] = new Iterator[(Int, Int)] {
	  private var i = dim1.low
	  private var j = dim2.low
	  def hasNext = i<=dim1.high && j<=dim2.high
	  def next = {
	    if (!hasNext) throw new NoSuchElementException("iterator overflow")
	    val result = (i, j)
	    if (j<dim2.high) { j += 1 }
	    else { i += 1; j = dim2.low }
	    result
	  }
  }

  override def canEqual(other: Any) = other.isInstanceOf[Index2]

  override def equals(other: Any) = other match {
    case that: Index2 => that.canEqual(this) && dim1 == that.dim1 && dim2 == that.dim2
    case _ => false
  }

  override def hashCode() = {
    val prime = 41
    prime * (prime + dim1.hashCode) + dim2.hashCode
  }

}

object Index2 {

	/**
	 * @param dim1 the "row" indices
	 * @param dim2 the "column" indices
	 * @return a 2-dimensional index range constructed from two 1-dimensional ones
	 */
	def apply(dim1: Index, dim2: Index) = new Index2(dim1, dim2)

	/**
	 * @return an empty 2-dimensional index range
	 */
	def empty () = new Index2(Index.empty(), Index.empty())

}