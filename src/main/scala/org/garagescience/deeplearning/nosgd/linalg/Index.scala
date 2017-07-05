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
 * Defines an immutable index range for vectors from {@code low} to {@code high}
 * (both inclusive) with step {@code 1}.
 *
 * @author h2b
 */
class Index private (val low: Int, val high: Int) extends Seq[Int] with Equals {

  import Index._

  require(Mindex<=low && high<=Maxdex, s"range overflow: [$low, $high]")

  override val isEmpty = low>high

  /**
   * @return the number of elements in this index range
   */
  def longSize: Long = if (isEmpty) 0L else high.toLong-low.toLong+1L

  /**
   * @return the number of elements in this index range
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
    low+i
  }

  /**
   * @return if i belongs to this index range
   */
  def contains (i: Int): Boolean = low<=i && i<=high

  /**
   * @return the intesection of this index range with that index range
   * @note The result may be empty.
   */
  def intersect (that: Index): Index =
    new Index(this.low max that.low, this.high min that.high)

  /**
   * @return the union of this index range with that index range
   * @note This is not a union of sets in mathematical sense since the result
   * will contain addtional elements if the original ranges are disjunct and not
   * adjacent.
   */
  def union (that: Index): Index = {
    if (this.isEmpty) return that
    if (that.isEmpty) return this
    new Index(this.low min that.low, this.high max that.high)
  }

  def iterator: Iterator[Int] = new Iterator[Int] {
	  private var i = low
	  def hasNext = i<=high
	  def next = {
	    if (!hasNext) throw new NoSuchElementException("iterator overflow")
	    i += 1
	    i-1
	  }
  }

  override def canEqual (other: Any) = other.isInstanceOf[Index]

  override def equals (other: Any) = other match {
    case that: Index => that.canEqual(this) && low == that.low && high == that.high
    case _ => false
  }

  override def hashCode() = {
    val prime = 41
    prime * (prime + low.hashCode) + high.hashCode
  }

}

object Index {

	/**
	 * Smallest index allowed.
	 */
	final val Mindex = Int.MinValue+2

	/**
	 * Greatest index allowed.
	 */
  final val Maxdex = Int.MaxValue-1

  /**
   * @param low must be equal to or greater than {@code Mindex}
   * @param high must be equal to or less than {@code Maxdex}
   * @return an index range `[low, high]`
   * @note if `low>high` the resulting index range becomes empty
   */
  def apply (low: Int, high: Int) = new Index(low, high)

  /**
   * @param high must be equal to or less than {@code Maxdex}
   * @return an index range `[1, high]`
   * @note if `high<1` the resulting index range becomes empty
   */
  def apply (high: Int) = new Index(1, high)

	/**
	 * @return an empty index range
	 */
	def empty () = new Index(Maxdex, Mindex)

}