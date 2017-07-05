package org.garagescience.deeplearning.nosgd.linalg

import junit.framework.Test

/**
 * @author jkk
 */
class DoubleVectorTest {

  import org.junit.Test
  import org.junit.Assert._

  private val testVector = Vector(1.0,2.0,3.0)
  private val testArray = Array(1.0,2.0,3.0)

  private val epsilon = 1e-6

  @Test
  def testUnaryPlus = {
    val u = testVector
    val v = +u
    assert(testArray, v, 1 to 3)
  }

  private def assert(expected: Array[Double], v: Vector[Double], index: Range) = {
    assertEquals(index.length, v.length)
    for (i <- index) {
      assertEquals(classOf[Double], v(i).getClass)
      assertEquals(expected(i-index.start), v(i), epsilon)
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
    val u2 = Vector.at(-1)(-4.0,1.0,2.0,3.0)
    val v = u1+u2
    assert(Array(-4,1,3,5,3), v, -1 to 3)
  }

  @Test
  def testMinus = {
    val u1 = testVector
    val u2 = Vector.at(0)(1.0,2.0,3.0)
    val v = u1-u2
    assert(Array(-1,-1,-1,3), v, 0 to 3)
  }

  @Test
  def testScalarProduct = {
    val u1 = testVector
    val u2 = Vector.at(0)(-1.0,-2,-4,-6,8)
    val s = u1*u2
    assertEquals(-28.0, s, epsilon)
  }

  @Test
  def testScaling = {
    val v = testVector*(-2.0)
    assert(Array(-2,-4,-6), v, 1 to 3)
    assertEquals(v, -2.0*testVector)
  }

  @Test
  def testNorm = {
    assertEquals(math.sqrt(14), testVector.norm, epsilon)
    assertEquals(0.0, Vector(0.0).norm, epsilon)
    assertEquals(math.sqrt(5), Vector.at(-2)(1.0,-2).norm, epsilon)
  }

  @Test
  def testIterator = {
		val v = testVector
    var sum = 0.0
    for (x <- v) sum += x
    assertEquals(6, sum, epsilon)
    var w = (for (x <- v) yield x).toArray
    for (i <- 1 to 3) assertEquals(v(i), w(i-1), epsilon)
  }

  @Test
  def testAt = {
    val v = testVector @@ -1
    assert(testArray, v, -1 to 1)
  }

  @Test
  def testMap = {
    val v = testVector
    val w = v.map((x: Double) => x*x: Double)
    assertEquals(v.length, w.length)
    for (i <- 1 to 3) assertEquals(v(i)*v(i), w(i), epsilon)
  }

  @Test
  def testMapAt = {
    val v = Vector.at(-1)(1.0,2.0,3.0)
    val w = v.map((x: Double) => x*x: Double)
    assertEquals(v.length, w.length)
    for (i <- -1 to 1) assertEquals(v(i)*v(i), w(i), epsilon)
  }

  @Test
  def testToString = {
    val v = testVector
    assertEquals("(1.0,2.0,3.0)@1", v.toString)
  }

  @Test
  def testEquals = {
    val u1 = testVector
    val u2 = Vector(1.0,2.0,3.0)
    val v = Vector.at(0)(1.0,2.0,3.0)
    val w = Vector(1.0,2.01,3.0)
    assertEquals(u1, u1)
    assertEquals(u1, u2)
    assertNotEquals(u1, v)
    assertNotEquals(u1, w)
    assertNotEquals(v, w)
  }

}