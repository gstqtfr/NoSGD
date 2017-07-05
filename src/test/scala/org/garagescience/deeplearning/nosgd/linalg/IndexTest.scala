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
class IndexTest extends FunSuite {

  val index = Index(1, 3)
  val empty = Index(Index.Maxdex, Index.Mindex)

  test("isEmpty") {
    assert(!index.isEmpty)
    assert(empty.isEmpty)
    assert(Index.empty().isEmpty)
  }

  test("size") {
    assertResult(3)(index.size)
    assertResult(0)(empty.size)
  }

  test("contains") {
    for (i <- 1 to 3) {
      assert(index.contains(i))
      assert(!empty.contains(i))
    }
    assert(!index.contains(0))
    assert(!index.contains(4))
  }

  test("intersect") {
    assert((index intersect empty).isEmpty)
    assert((empty intersect index).isEmpty)
	  assertResult(index)(index intersect index)
	  assertResult(Index(1,2))(index intersect Index(1,2))
	  assertResult(Index(2,3))(index intersect Index(2,4))
	  assert((index intersect Index(4,5)).isEmpty)
  }

  test("union") {
    assertResult(index)(index union empty)
    assertResult(index)(empty union index)
	  assertResult(index)(index union index)
	  assertResult(index)(index union Index(1,2))
	  assertResult(Index(1,4))(index union Index(2,4))
	  assertResult(Index(1,10))(index union Index(7,10))
  }

  test("equals") {
    assert(index==index)
    assert(empty==empty)
    assert(index!=empty)
    assert(index!=Index.empty())
    assert(index==Index(1, 3))
    assert(index!=Index(1, 2))
    assert(index!=Index(2, 4))
    assert(index!=Index(4, 6))
  }

  test("iterator") {
    var k = 1
    for (i <- index) { assertResult(k)(i); k += 1 }
    for (i <- empty) fail("executed by empty iterator")
  }

  test("apply") {
    assertResult(1)(index(0))
    assertResult(2)(index(1))
    assertResult(3)(index(2))
  }

  test("exception") {
    intercept[IllegalArgumentException](Index(Index.Mindex, Index.Maxdex+1))
    intercept[IndexOutOfBoundsException](index(3))
    intercept[IndexOutOfBoundsException](empty(0))
  }

}