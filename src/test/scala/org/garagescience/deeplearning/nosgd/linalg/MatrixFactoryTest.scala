package org.garagescience.deeplearning.nosgd.linalg

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.garagescience.deeplearning.nosgd.linalg.Matrix.AtRow

@RunWith(classOf[JUnitRunner])
class MatrixFactoryTest extends FunSuite {

  private val a = Matrix(Vector(11,12,13), Vector(21,22,23))

  private val m = 2 //number of rows
  private val n = 3 //number of columns

  test("Standard index ranges are correct") {
    assertResult(1)(a.index.dim1.low)
    assertResult(2)(a.index.dim1.high)
    assertResult(1)(a.index.dim2.low)
    assertResult(3)(a.index.dim2.high)
  }

  test("At index ranges are correct") {
    implicit val at = AtRow(-1)
    val b = Matrix(Vector(11,12,13), Vector(21,22,23)) atCol 0
    assertResult(-1)(b.index.dim1.low)
    assertResult(0)(b.index.dim1.high)
    assertResult(0)(b.index.dim2.low)
    assertResult(2)(b.index.dim2.high)
  }

  test("Matrix has correct elements") {
    for {
      i <- 1 to m
      j <- 1 to n
    } assertResult(10*i+j)(a(i,j))
  }

  test("At yields correct values") {
    val b = Matrix.atRow(-1)(Vector(11,12,13), Vector(21,22,23)) atCol 0
    for {
      i <- -1 to 0
      j <- 0 to 2
    } assertResult(10*(i+2)+j+1)(b(i,j))
  }

  test("Values beyond concrete index range are zero") {
    assertResult(0)(a(0,0))
    assertResult(0)(a(1,0))
    assertResult(0)(a(1,4))
    assertResult(0)(a(3,1))
  }

  test("Empty matrix is producible") {
    val b = Matrix[Int]()
    assertResult(0)(b.height)
    assertResult(0)(b.width)
  }

  test("Create matrix by function") {
    val b = Matrix((i: Int) => Vector(i*10+1,i*10+2,i*10+3), 1, 2)
    for {
      i <- 1 to m
      j <- 1 to n
    } assertResult(10*i+j)(b(i,j))
  }

  test("Element type") {
    val intMat = a
    assert(intMat(2,3).isInstanceOf[Int])
    val charMat = Matrix(Vector('a','b'), Vector('c','d'))
    assert(charMat(1,2).isInstanceOf[Char])
  }

  test("scal0 is correct") {
    assertResult(0.0)(Matrix.scal0[Double])
    assertResult(0)(Matrix.scal0[Int])
    assertResult(0L)(Matrix.scal0[Long])
  }

  test("scal1 is correct") {
    assertResult(1.0)(Matrix.scal1[Double])
    assertResult(1)(Matrix.scal1[Int])
    assertResult(1L)(Matrix.scal1[Long])
  }

}