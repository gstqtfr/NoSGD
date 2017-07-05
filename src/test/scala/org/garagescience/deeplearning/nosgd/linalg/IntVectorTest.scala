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
class IntVectorTest {

  import org.junit.Test
  import org.junit.Assert._

  private val testVector = Vector(1,2,3)
  private val testArray = Array(1,2,3)

  private val epsilon = 1e-6

  @Test
  def testUnaryPlus = {
    val u = testVector
    val v = +u
    assert(testArray, v, 1 to 3)
  }

  private def assert(expected: Array[Int], actual: Vector[Int], index: Range) = {
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
    val u2 = Vector.at(0)(1,2,3)
    val v = u1+u2
    assert(Array(1,3,5,3), v, 0 to 3)
  }

  @Test
  def testMinus = {
    val u1 = testVector
    val u2 = Vector.at(0)(1,2,3)
    val v = u1-u2
    assert(Array(-1,-1,-1,3), v, 0 to 3)
  }

  @Test
  def testScalarProduct = {
    val u1 = testVector
    val u2 = Vector.at(0)(-1,-2,-4,-6,8)
    val s = u1*u2
    assertEquals(-28, s)
  }

  @Test
  def testScaling = {
    val v = testVector*(-2)
    assert(Array(-2,-4,-6), v, 1 to 3)
    assertEquals(v, -2*testVector)
  }

  @Test
  def testNorm = {
    assertEquals(math.sqrt(14), testVector.norm, epsilon)
    assertEquals(0.0, Vector(0).norm, epsilon)
    assertEquals(math.sqrt(5), Vector.at(-2)(1,-2).norm, epsilon)
  }

  @Test
  def testIterator = {
		val v = testVector
    var sum = 0
    for (x <- v) sum += x
    assertEquals(6, sum)
    var w = (for (x <- v) yield x).toArray
    for (i <- 1 to 3) assertEquals(v(i), w(i-1))
  }

  @Test
  def testAt = {
    val v = testVector @@ -1
    assert(testArray, v, -1 to 1)
  }

  @Test
  def testShorten = {
    val v = Vector(1,2,3)
    val v1 = Vector(0,1,2,3)@@0
    val v2 = Vector(1,2,3,0)
    val v3 = Vector(0,0,1,2,3,0,0)@@(-1)
    assertTrue(v1.shorten.isInstanceOf[Vector[Int]])
    assertTrue(v2.shorten.isInstanceOf[Vector[Int]])
    assertTrue(v3.shorten.isInstanceOf[Vector[Int]])
    assertEquals(v, v1.shorten)
    assertEquals(v, v2.shorten)
    assertEquals(v, v3.shorten)
    val v0 = Vector[Int]()
    assertTrue(v0.shorten.isInstanceOf[Vector[Int]])
    assertTrue(v0.shorten.isEmpty)
    val v0a = Vector[Int](0,0,0)
    assertTrue(v0a.shorten.isInstanceOf[Vector[Int]])
    assertTrue(v0a.shorten.isEmpty)
  }

  @Test
  def testWiden = {
		val v = Vector(1,2,3)
    val v1 = Vector(0,1,2,3)@@0
    val v2 = Vector(1,2,3,0)
    val v3 = Vector(0,0,1,2,3,0,0)@@(-1)
    assertTrue(v1.widen(Index(0,0)).isInstanceOf[Vector[Int]])
    assertTrue(v2.widen(Index(2,4)).isInstanceOf[Vector[Int]])
    assertTrue(v3.widen(Index(-1,5)).isInstanceOf[Vector[Int]])
    assertEquals(v1, v.widen(Index(0,0)))
    assertEquals(v2, v.widen(Index(2,4)))
    assertEquals(v3, v.widen(Index(-1,5)))
    val v0 = Vector[Int]()
    assertTrue(v0.widen(Index(-10,10)).isInstanceOf[Vector[Int]])
    assertTrue(v0.widen(Index(-10,10)).isZero)
    assertEquals(Vector(0,0,0,0,0)@@(-2), v0.widen(Index(-2,2)))
  }

  @Test
  def testSimilarity = {
		val v = Vector(1,0,3)
    val v1 = Vector(0,1,0,3)@@0
    val v2 = Vector(1,0,3,0)
    val v3 = Vector(0,0,1,0,3,0,0)@@(-1)
    assertTrue(v~~v1)
    assertTrue(v~~v2)
    assertTrue(v~~v3)
    assertTrue(v1~~v2)
    assertTrue(v1~~v3)
    assertTrue(v2~~v3)
    val w = Vector(1,2,3)
    assertFalse(v~~w)
    val v0a = Vector[Int]()
    val v0b = Vector[Int]()
    val v0c = Vector[Int](0,0,0,0)
    assertTrue(v0a~~v0b)
    assertTrue(v0a~~v0c)
    assertFalse(v~~v0a)
    assertFalse(v0a~~v)
  }

  @Test
  def testMap = {
    val v = testVector
    val w = v.map((x: Int) => x*x: Int)
    assertEquals(v.length, w.length)
    for (i <- 1 to 3) assertEquals(v(i)*v(i), w(i))
  }

  @Test
  def testMapAt = {
    val v = Vector.at(-1)(1,2,3)
    val w = v.map((x: Int) => x*x: Int)
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
    val u2 = Vector(1,2,3)
    val v = Vector.at(0)(1,2,3)
    val w = Vector(1,4,3)
    assertEquals(u1, u1)
    assertEquals(u1, u2)
    assertNotEquals(u1, v)
    assertNotEquals(u1, w)
    assertNotEquals(v, w)
  }

}