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

import junit.framework.Test

/**
 * @author h2b
 */
class NumericVectorTest {

  import org.junit.Test
  import org.junit.Assert._

  private val testVector = Vector(1L,2,3)
  private val testArray = Array(1L,2,3)

  private val epsilon = 1e-6

  @Test
  def testUnaryPlus = {
    val u = testVector
    val v = +u
    assert(testArray, v, 1 to 3)
  }

  private def assert(expected: Array[Long], actual: Vector[Long], index: Range) = {
    assertEquals(index.length, actual.length)
    for (i <- index) {
      assertEquals(expected(i-index.start), actual(i))
    }
  }

  @Test
  def testUnaryMinus = {
    val u = testVector
    val v = -u
    assert(Array(-1,-2,-3), v, 1 to 3)
  }

  @Test
  def testPlus = {
    val u1 = testVector
    val u2 = Vector.at(0)(1L,2,3)
    val v = u1+u2
    assert(Array(1,3,5,3), v, 0 to 3)
  }

  @Test
  def testMinus = {
    val u1 = testVector
    val u2 = Vector.at(-1)(-4L,1,2,3)
    val v = u1-u2
    assert(Array(4,-1,-1,-1,3), v, -1 to 3)
  }

  @Test
  def testScalarProduct = {
    val u1 = testVector
    val u2 = Vector.at(0)(-1L,-2,-4,-6,8)
    val s = u1*u2
    assertEquals(-28, s)
  }

  @Test
  def testScaling = {
    val v = testVector*(-2L)
    assert(Array(-2,-4,-6), v, 1 to 3)
    assertEquals(v, -2L*testVector)
  }

  @Test
  def testNorm = {
    assertEquals(math.sqrt(14), testVector.norm, epsilon)
    assertEquals(0.0, Vector(0L).norm, epsilon)
    assertEquals(math.sqrt(5), Vector.at(-2)(1L,-2).norm, epsilon)
  }

  @Test
  def testIterator = {
		val v = testVector
    var sum = 0L
    for (x <- v) sum += x
    assertEquals(6L, sum)
    var w = (for (x <- v) yield x).toArray
    for (i <- 1 to 3) assertEquals(v(i), w(i-1))
  }

  @Test
  def testAt = {
    val v = testVector @@ -1
    assert(testArray, v, -1 to 1)
  }

  @Test
  def testMap = {
    val v = testVector
    val w = v.map((x: Long) => x*x: Long)
    assertEquals(v.length, w.length)
    for (i <- 1 to 3) assertEquals(v(i)*v(i), w(i))
  }

  @Test
  def testMapAt = {
    val v = Vector.at(-1)(1L,2,3)
    val w = v.map((x: Long) => x*x: Long)
    assertEquals(v.length, w.length)
    for (i <- -1 to 1) assertEquals(v(i)*v(i), w(i))
  }

  @Test
  def testToString = {
    val v = testVector
    assertEquals("(1,2,3)@1", v.toString)
  }

  @Test
  def testEquals = {
    val u1 = testVector
    val u2 = Vector(1L,2,3)
    val v = Vector.at(0)(1L,2,3)
    val w = Vector(1L,4,3)
    assertEquals(u1, u1)
    assertEquals(u1, u2)
    assertNotEquals(u1, v)
    assertNotEquals(u1, w)
    assertNotEquals(v, w)
  }

}