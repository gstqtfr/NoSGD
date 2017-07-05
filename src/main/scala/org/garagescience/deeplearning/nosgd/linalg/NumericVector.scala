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
 * An algebraic vector consisting of implicitly numeric elements with index range
 * {@code [startIndex, startIndex+length-1]}.
 *
 * @author h2b
 */
abstract class NumericVector [E] protected (protected val startIndex: Int, protected val elems: Seq[E])
    (protected implicit val num: Numeric[E], protected implicit val elemTag: ClassTag[E])
    extends Vector[E] {

  protected object op extends ScalarOps {
    def negate (x: E): E = num.negate(x)
    def plus (x: E, y: E) = num.plus(x, y)
    def times (x: E, y: E) = num.times(x, y)
  }

  def norm: Double = math.sqrt(num.toDouble(this*this))

}

object NumericVector {

  def apply [E : Numeric : ClassTag] (n: Int, elems: Seq[E]) =
    new NumericVector(n, elems) with SimpleVectorStore[E]

}

object NumericLongVectorFactory extends VectorFactory[Long] {

  val zero = 0: Long
  val one = 1: Long

  def create (startIndex: Int, elems: Seq[Long]): Vector[Long] =
    NumericVector(startIndex, elems)

}

object NumericIntVectorFactory extends VectorFactory[Int] {

  val zero = 0: Int
  val one = 1: Int

  def create (startIndex: Int, elems: Seq[Int]): Vector[Int] =
    NumericVector(startIndex, elems)

}

object NumericShortVectorFactory extends VectorFactory[Short] {

  val zero = 0: Short
  val one = 1: Short

  def create (startIndex: Int, elems: Seq[Short]): Vector[Short] =
    NumericVector(startIndex, elems)

}

object NumericByteVectorFactory extends VectorFactory[Byte] {

  val zero = 0: Byte
  val one = 1: Byte

  def create (startIndex: Int, elems: Seq[Byte]): Vector[Byte] =
    NumericVector(startIndex, elems)

}

object NumericCharVectorFactory extends VectorFactory[Char] {

  val zero = 0: Char
  val one = 1: Char

  def create (startIndex: Int, elems: Seq[Char]): Vector[Char] =
    NumericVector(startIndex, elems)

}

object NumericFloatVectorFactory extends VectorFactory[Float] {

  val zero = 0: Float
  val one = 1: Float

  def create (startIndex: Int, elems: Seq[Float]): Vector[Float] =
    NumericVector(startIndex, elems)

}

object NumericDoubleVectorFactory extends VectorFactory[Double] {

  val zero = 0: Double
  val one = 1: Double

  def create (startIndex: Int, elems: Seq[Double]): Vector[Double] =
    NumericVector(startIndex, elems)

}

