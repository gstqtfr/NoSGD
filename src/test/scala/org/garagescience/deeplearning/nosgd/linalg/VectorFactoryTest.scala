package org.garagescience.deeplearning.nosgd.linalg

import org.junit.Assert
import org.garagescience.deeplearning.nosgd.linalg.Vector.At

/**
 * @author h2b
 */
class VectorFactoryTest {

  import org.junit.Test
  import org.junit.Assert._

  private val testVector = Vector(1.0,2.0,3.0)
  private val testArray = Array(1.0,2.0,3.0)

  private val epsilon = 1e-6

  @Test
  def testCreate = {
    assert(testArray, testVector, 1 to 3)
  }

  private def assert(expected: Array[Double], v: Vector[Double], index: Range) = {
    assertEquals(index.length, v.length)
    for (i <- index) {
      assertEquals(classOf[Double], v(i).getClass)
      assertEquals(expected(i-index.start), v(i), epsilon)
    }
  }

  @Test
  def testAt = {
    implicit val at = At(0)
    val v = Vector(1.0,2.0,3.0)
    assert(testArray, v, 0 to 2)
  }

  @Test
  def testRange = {
    val v = testVector
    assertEquals(1, v.index.low)
    assertEquals(3, v.index.high)
  }

  @Test
  def testApply = {
      val v = testVector
      assertEquals(0, v(-1), epsilon)
      assertEquals(0, v(0), epsilon)
      assertEquals(1, v(1), epsilon)
      assertEquals(2, v(2), epsilon)
      assertEquals(3, v(3), epsilon)
      assertEquals(0, v(4), epsilon)
  }

  @Test
  def testFunctionCreate = {
    val v = Vector((i: Int) => i*i: Double, 1, 5)
    assert(Array(1,4,9,16,25), v, 1 to 5)
  }

  @Test
  def testEmpty = {
    val v = Vector[Double]()
    assertEquals(0, v.length)
  }

  @Test
  def testToArray = {
		val v = testVector
    val a = v.toArray
    assertEquals(3, a.length)
    for (i <- 1 to 3) assertEquals(v(i), a(i-1), epsilon)
  }

  @Test
  def testToList = {
    val v = testVector
    val l = v.toList
    assertEquals(3, l.length)
    for (i <- 1 to 3) assertEquals(v(i), l(i-1), epsilon)
  }

  @Test
  def testType = {
	  val charVec = Vector('a', 'b')
    assertTrue(charVec(2).isInstanceOf[Char])
    val byteVec = Vector(1.toByte, 2.toByte)
    assertTrue(byteVec(2).isInstanceOf[Byte])
    val shortVec = Vector(1.toShort, 2.toShort)
    assertTrue(shortVec(2).isInstanceOf[Short])
    val intVec = Vector(1, 2)
    assertTrue(intVec(2).isInstanceOf[Int])
    val longVec = Vector(1L, 2)
    assertTrue(longVec(2).isInstanceOf[Long])
    val floatVec = Vector(1f, 2)
    assertTrue(floatVec(2).isInstanceOf[Float])
    val doubleVec = Vector(1.0, 2)
    assertTrue(doubleVec(2).isInstanceOf[Double])
  }

  @Test(expected=classOf[UnsupportedOperationException])
  def testException = {
    val stringVec = Vector("s1", "s2")
  }

  @Test
  def testScal0 = {
    assertEquals(0.0, Vector.scal0[Double], epsilon)
    assertEquals(0, Vector.scal0[Int])
    assertEquals(0L, Vector.scal0[Long])
  }

  @Test
  def testScal1 = {
    assertEquals(1.0, Vector.scal1[Double], epsilon)
    assertEquals(1, Vector.scal1[Int])
    assertEquals(1L, Vector.scal1[Long])
  }

}