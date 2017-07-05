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

package org.garagescience.deeplearning.nosgd

import scala.reflect.ClassTag

/**
 * @author h2b
 */
package object linalg {

	/**
	 * Defines vector and matrix operations on the scalar `s` as left operand.
	 *
	 * @param <E> the vector or matrix element's type
	 * @param <F> the scalar's type
	 */
	implicit class ScalarOps [E : ClassTag] (private val s: E) {

	  /**
	   * Scales the specified vector by `s`.
	   *
	   * @param v the vector
	   * @return `s`*`v`
	   */
    def * (v: Vector[E]): Vector[E] = v * s

	  /**
	   * Scales the specified matrix by `s`.
	   *
	   * @param a the matrix
	   * @return `s`*`v`
	   */
    def * (a: Matrix[E]): Matrix[E] = a * s

  }

	/**
	 * Defines matrix operations on the vector `v` as left operand.
	 *
	 * @param <E> the matrix element's type
	 * @param <F> the vectors's element type
	 */
	implicit class VectorOps [E : ClassTag] (private val v: Vector[E]) {

		/**
		 * Returns the product of `v` and the matrix.
		 *
	   * @note This should be named `*`, but there is an ambiguity with
	   * `ScalarOps`. Until this problem is solved, use `**`.
	   *
		 * @param a the matrix to be multiplied
		 * @return `v`*`a`
		 */
    def ** (a: Matrix[E]): Vector[E] = {
  		val vbuilder = VectorBuilder[E]() at v.index.low
			for (col  <- a.colIterator) vbuilder += v*col
			vbuilder.result()
    }

  }

}