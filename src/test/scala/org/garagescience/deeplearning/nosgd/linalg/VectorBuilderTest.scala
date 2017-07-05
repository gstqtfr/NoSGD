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

/**
 * @author h2b
 */
class VectorBuilderTest {

  import org.junit.Test
  import org.junit.Assert._

  private val epsilon = 1e-6

  @Test
  def testBuilder = {
    val builder = VectorBuilder[Double]()
    builder += 1.0
    builder += 2.0
    builder += 3.0
    val v = builder.result()
    assertTrue(v.isInstanceOf[Vector[Double]])
    assert(Array(1,2,3), v, 1 to 3)
  }

  private def assert(expected: Array[Double], v: Vector[Double], index: Range) = {
    assertEquals(index.length, v.length)
    for (i <- index) {
      assertEquals(classOf[Double], v(i).getClass)
      assertEquals(expected(i-index.start), v(i), epsilon)
    }
  }

  @Test
  def testBuilderAt = {
    val builder = VectorBuilder[Double]()
    builder += 1.0
    builder += 2.0
    builder += 3.0
    builder at 11
    val v = builder.result()
    assertTrue(v.isInstanceOf[Vector[Double]])
    assert(Array(1,2,3), v, 11 to 13)
  }

  @Test
  def testBuilderUpdate = {
    val builder = VectorBuilder[Double]()
    builder(0) = 0.0
    builder(3) = +3.0
    builder(-3) = -3.0
    val v = builder.result()
    assertTrue(v.isInstanceOf[Vector[Double]])
    assert(Array(-3,0,0,0,0,0,3), v, -3 to 3)
  }

}