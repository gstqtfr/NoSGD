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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class ZeroMatrixTest extends FunSuite {

  private val zeromat = Matrix[Int]()
  private val zerovec = Vector[Int]()
  private val zeroscal = Matrix.scal0[Int]
  private val othermat = Matrix(Vector(11,12,13), Vector(21,22,23))
  private val othervec = Vector(1,2,3)

  test("size is empty") {
    assertResult(0)(zeromat.height)
    assertResult(0)(zeromat.width)
  }

  test(s"index is empty") {
	  assert(zeromat.index.isEmpty)
  }

  test("is square") {
    assert(zeromat.isSquare)
  }

  test("is zero") {
    assert(zeromat.isZero)
    assert(!othermat.isZero)
  }

  test("applying yields zero") {
    assertResult(zeroscal)(zeromat(1,1))
    assert(zeromat(1).isZero)
    assert(zeromat.row(1).isZero)
    assert(zeromat.col(1).isZero)
    //TODO: put this into docu:
    /*
     * Another reason to have isZero: The index ranges of the resulting zerovecs
     * are undefined. This is also true for othermat*zeromat and zeromat*somevec
     * below.
     */
  }

  test("unary plus/minus yields zeromat") {
    assertResult(zeromat)(+zeromat)
    assertResult(zeromat)(-zeromat)
  }

  test("addition and subtraction is neutral") {
	  assertResult(zeromat)(zeromat+zeromat)
	  assertResult(zeromat)(zeromat-zeromat)
	  assertResult(othermat)(othermat+zeromat)
	  assertResult(othermat)(zeromat+othermat)
	  assertResult(othermat)(othermat-zeromat)
	  assertResult(-othermat)(zeromat-othermat)
  }

  test("matrix product is zero") {
    assertResult(zeromat)(zeromat*zeromat)
    assertResult(zeromat)(zeromat*othermat)
    assert((othermat*zeromat).isZero)
  }

  test("matrix*vector is zero") {
    assert((zeromat*zerovec).isZero)
    assert((zeromat*othervec).isZero)
    assert((othermat*zerovec).isZero)
    //TODO: put this into docu:
    /*
     * Here is the reason why isZero cannot just test if the index range is
     * empty: othermat*zerovec yields a Vector(0,0) (concrete zeroes). To fix
     * this, we could change the builder to check, if an added element is zero,
     * and in this case to not widen the index range if it would be necessary
     * (i.e., not adding zeroes before or after the index range it has so far);
     * again, this might cause inconsistencies (e.g., Vector(f,1,3)) could have
     * not the index range expected.)
     */
  }

  test("scaling yields zeromat") {
    assertResult(zeromat)(zeromat*2)
  }

  test("transposed if zeromat") {
    assert(zeromat.transposed().isZero)
  }

  test("row and column sums are zero") {
    assert(zeromat.rowSum().isZero)
    assert(zeromat.colSum().isZero)
  }

  test("iterators are empty") {
    for (row ← zeromat) fail("executed by empty iterator")
    for (row ← zeromat.rowIterator) fail("executed by empty row iterator")
    for (col ← zeromat.colIterator) fail("executed by empty col iterator")
    for (elem ← zeromat.elemIterator) fail("executed by empty elem iterator")
  }

  test("at yields zero matrix") {
	  assert(zeromat.atRow(-1).isZero)
	  assert(zeromat.atCol(-1).isZero)
	  assert((zeromat@@(-1,-1)).isZero)
  }

  test("string representation is okay") {
    assertResult("()@1,2147483646")(zeromat.toString)
  }

  test(s"equals is consistent") {
	  assert(zeromat==zeromat)
	  assert(zeromat!=othermat)
	  assert(othermat!=zeromat)
	  assertResult(Matrix[Int]())(Matrix[Int]())
  }

  test("map function yields zeromat") {
    assertResult(zeromat)(zeromat.map(_*2))
  }

}