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
 * A storage for vector elements.
 *
 * @param <E> type of the elements
 * @since 2.0.0
 * @author h2b
 */
trait VectorStore [E] {

  protected implicit val elemTag: ClassTag[E]

	val index: Index

  /**
   * @param i
   * @return the element with index {@code i}
   */
  def apply (i: Int): E

  protected val dataHashCode: Int

  override def hashCode = {
    val prime = 31
    var result = 1
    result = prime*result+index.low
    result = prime*result+index.high
    result = prime*result+dataHashCode
    result.toInt
  }

}

/**
 * Implementation trait that uses `scala.collection.immutable.Vector` as storage.
 *
 * @param <E> type of the elements
 * @since 2.0.0
 * @author h2b
 */
trait SimpleVectorStore [E] extends VectorStore[E] {

  protected val startIndex: Int
  protected val elems: Seq[E]

  private val data = scala.collection.immutable.Vector(elems: _*)

  protected val dataHashCode: Int = data.hashCode

  val index = Index(startIndex, startIndex+data.length -1)

  private val zero = Vector.scal0[E]

  def apply (i: Int): E = {
		if (index.contains(i)) data(i-index.low) else zero
  }

}
