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
 * An algebraic vector consisting of {@code Double} elements with index range
 * {@code [startIndex, startIndex+length-1]}.
 *
 * @author h2b
 */
abstract class DoubleVector protected (protected val startIndex: Int, protected val elems: Seq[Double]) extends Vector[Double] {

  protected implicit val elemTag = ClassTag.Double

  protected object op extends ScalarOps {
    def negate (x: Double): Double = -x
    def plus (x: Double, y: Double) = x+y
    def times (x: Double, y: Double) = x*y
  }

  def norm: Double = math.sqrt(this*this)

}

object DoubleVector {

  def apply (n: Int, elems: Seq[Double]) = new DoubleVector(n, elems) with SimpleVectorStore[Double]

}

object DoubleVectorFactory extends VectorFactory[Double] {

  val zero = 0.0
  val one = 1.0

  def create (startIndex: Int, elems: Seq[Double]): Vector[Double] =
    DoubleVector(startIndex, elems)

}
