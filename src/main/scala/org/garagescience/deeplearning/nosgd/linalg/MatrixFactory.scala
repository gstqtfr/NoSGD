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
 * @author h2b
 */
trait MatrixFactory {

  def create [E : ClassTag] (rowStart: Int, elems: Seq[Vector[E]]): Matrix[E]

}

object MatrixFactory {

  def apply [E : ClassTag] (lowRow: Int, seq: Seq[Vector[E]]) =
	  RowMatrixFactory.create(lowRow, seq)

}