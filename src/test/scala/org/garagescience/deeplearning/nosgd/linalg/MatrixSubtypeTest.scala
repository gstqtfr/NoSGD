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

import scala.collection.IterableLike

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MatrixSubtypeTest extends FunSuite {

  private val a1 = Matrix2x2(11,12,21,22)
  private val a2 = Matrix2x2(111,112,121,122)

  test("atRow is dummy on Matrix2x2s but yields correct type") {
    val b = a1 atRow 3
	  assertResult(1)(b.index.dim1.low)
	  assertResult(classOf[Matrix2x2])(b.getClass)
  }

  test("atRow yields correct result") {
    val b = a1 atRow 3
	  assertResult(a1)(b)
  }

  test("unary ops yield correct type") {
    val b1 = +a1
    val b2 = -a1
	  assertResult(classOf[Matrix2x2])(b1.getClass)
	  assertResult(classOf[Matrix2x2])(b2.getClass)
  }

  test("unary ops yield correct result") {
    val b1 = +a1
    val b2 = -a1
	  assertResult(11)(b1.a11)
	  assertResult(12)(b1.a12)
	  assertResult(21)(b1.a21)
	  assertResult(22)(b1.a22)
	  assertResult(-11)(b2.a11)
	  assertResult(-12)(b2.a12)
	  assertResult(-21)(b2.a21)
	  assertResult(-22)(b2.a22)
  }

  test("multiplication by scalar from right yields correct type") {
    val b = a1*5
	  assertResult(classOf[Matrix2x2])(b.getClass)
  }

  test("multiplication by scalar from right yields correct result") {
    val b = a1*5
	  assertResult(55)(b.a11)
	  assertResult(60)(b.a12)
	  assertResult(105)(b.a21)
	  assertResult(110)(b.a22)
  }

  test("multiplication by scalar from left yields correct result and type") {
    val b = 5*a1
    assertResult(Matrix2x2(55,60,105,110))(b)
	  assertResult(classOf[Matrix2x2])(b.getClass)
  }

  test("transposed yields correct type") {
    val b = a1.transposed()
	  assertResult(classOf[Matrix2x2])(b.getClass)
  }

  test("transposed yields correct result") {
    val b = a1.transposed()
	  assertResult(a1.a11)(b.a11)
	  assertResult(a1.a21)(b.a12)
	  assertResult(a1.a12)(b.a21)
	  assertResult(a1.a22)(b.a22)
  }

  test("map yields correct result and type") {
    val b = a1.map(_*5)
    assertResult(Matrix2x2(55,60,105,110))(b)
	  assertResult(classOf[Matrix2x2])(b.getClass)
  }

  test("binary additon, subtraction and multiplication yield correct type") {
    val b1 = a1+a2
    val b2 = a1-a2
    val b3 = a1*a2
	  assertResult(classOf[Matrix2x2])(b1.getClass)
	  assertResult(classOf[Matrix2x2])(b2.getClass)
	  assertResult(classOf[Matrix2x2])(b3.getClass)
  }

  test("binary additon, subtraction and multiplication yield correct result") {
    val b1 = a1+a2
    val b2 = a1-a2
    val b3 = a1*a2
    assertResult(Matrix2x2(122,124,142,144))(b1)
    assertResult(Matrix2x2(-100,-100,-100,-100))(b2)
    assertResult(Matrix2x2(2673,2696,4993,5036))(b3)
  }

  test("multiplication by vector yields correct type") {
    val v = Vector(1,2)
    val w = a1*v
	  assertResult(v.getClass)(w.getClass)
  }

  test("multiplication by vector yields correct result") {
    val v = Vector(1,2)
    val w = a1*v
	  assertResult(Vector(35,65))(w)
  }

  test("apply yields correct result") {
	  assertResult(11)(a1(1,1))
	  assertResult(12)(a1(1,2))
	  assertResult(21)(a1(2,1))
	  assertResult(22)(a1(2,2))
  }

}

class Matrix2x2 private (val a11: Int, val a12: Int, val a21: Int, val a22: Int) extends
    RowMatrix[Int](1, Seq(Vector(a11,a12), Vector(a21,a22))) with IterableLike[Vector[Int], Matrix2x2] with
    MatrixLike[Int, Matrix2x2] with RowMatrixStore[Int] {

  override protected[this] def newBuilder: MatrixBuilder[Vector[Int],Matrix2x2] = Matrix2x2.newBuilder

}

object Matrix2x2 {

  def apply (a11: Int, a12: Int, a21: Int, a22: Int) = new Matrix2x2(a11,a12,a21,a22)

  def newBuilder: MatrixBuilder[Vector[Int], Matrix2x2] =
    new MatrixBuilder[Vector[Int], Matrix2x2] {
    	protected val zerovec = Vector[Int]()
		  def result: Matrix2x2 = {
				  require(elems.length==2)
				  Matrix2x2(elems(0)(1),elems(0)(2),elems(1)(1),elems(1)(2))
      }
  }

  implicit def canBuildFrom: MatrixCanBuildFrom[Matrix2x2, Vector[Int], Matrix2x2] =
    new MatrixCanBuildFrom[Matrix2x2, Vector[Int], Matrix2x2] {
	    def apply (): MatrixBuilder[Vector[Int],Matrix2x2] = newBuilder
      def apply (from: Matrix2x2): MatrixBuilder[Vector[Int],Matrix2x2] = newBuilder
  }

	implicit class ScalarOps (s: Int) {
    def * (v: Matrix2x2): Matrix2x2 = v * s
	}

}
