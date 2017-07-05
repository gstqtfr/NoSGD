package org.garagescience.deeplearning.nosgd.linalg

import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MatrixBuilderTest extends FunSuite {

	implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(1e-6)

  private def check (expected: Array[Array[Double]], actual: Matrix[Double], rowRange: Range, colRange: Range) = {
	  assert(rowRange.size == actual.height)
	  assert(colRange.size == actual.width)
	  for {
	    i <- rowRange
	    j <- colRange
	  } assert(expected(i-rowRange.start)(j-colRange.start) === actual(i,j))
  }

	test("Building a matrix at default start indices is OK") {
	  val v1 = Vector(1.0,2.0,3.0)
	  val v2 = Vector(4.0,5.0,6.0)
	  val builder = MatrixBuilder[Double]()
	  builder += v1
	  builder += v2
	  val a = builder.result()
	  assert(a.isInstanceOf[Matrix[Double]])
	  check(Array(Array(1.0,2.0,3.0),Array(4.0,5.0,6.0)), a, 1 to 2, 1 to 3)
	}

	test("Building a matrix at explicit row start is OK") {
	  val v1 = Vector(1.0,2.0,3.0)
	  val v2 = Vector(4.0,5.0,6.0)
	  val builder = MatrixBuilder[Double]()
	  builder += v1
	  builder += v2
	  builder at 10
	  val a = builder.result()
	  assert(a.isInstanceOf[Matrix[Double]])
	  check(Array(Array(1.0,2.0,3.0),Array(4.0,5.0,6.0)), a, 10 to 11, 1 to 3)
	}

	test("Building a matrix using updates is OK") {
	  val v1 = Vector(1.0,2.0,3.0)
	  val v2 = Vector(4.0,5.0,6.0)
	  val builder = MatrixBuilder[Double]()
	  builder(+1) = v2
	  builder(-1) = v1
	  val a = builder.result()
	  assert(a.isInstanceOf[Matrix[Double]])
	  check(Array(Array(1.0,2.0,3.0),Array(0.0, 0.0, 0.0),Array(4.0,5.0,6.0)), a, -1 to 1, 1 to 3)
	}

}