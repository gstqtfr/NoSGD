package org.garagescience.deeplearning.nosgd.linalg

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class Index2Test extends FunSuite {

  val index2 = Index2(Index(1,2), Index(1,3))
  val empty2 = Index2(Index(1,2), Index(2,1))

  test("size") {
    assertResult(6)(index2.size)
    assertResult(0)(empty2.size)
  }

  test("contains") {
    for {
      i <- 1 to 2
      j <- 1 to 3
    }
    {
      assert(index2.contains(i,j))
      assert(!empty2.contains(i,j))
    }
    for (j <- 1 to 3) {
    	assert(!index2.contains(0,j))
    	assert(!index2.contains(3,j))
    }
    for (i <- 1 to 2) {
    	assert(!index2.contains(i,0))
    	assert(!index2.contains(i,4))
    }
    assert(!index2.contains(0, 0))
  }

  test("intersect") {
    assert((index2 intersect empty2).isEmpty)
    assert((empty2 intersect index2).isEmpty)
	  assertResult(index2)(index2 intersect index2)
	  assertResult(Index2(Index(1,1), Index(1,2)))(index2 intersect Index2(Index(1,1), Index(1,2)))
	  assertResult(Index2(Index(2,2), Index(2,3)))(index2 intersect Index2(Index(2,3), Index(2,4)))
	  assert((index2 intersect Index2(Index(1,2), Index(4,5))).isEmpty)
  }

  test("union") {
    assertResult(index2)(index2 union empty2)
    assertResult(index2)(empty2 union index2)
	  assertResult(index2)(index2 union index2)
	  assertResult(index2)(index2 union Index2(Index(1,1), Index(1,2)))
	  assertResult(Index2(Index(1,3), Index(1,4)))(index2 union Index2(Index(2,3), Index(2,4)))
	  assertResult(Index2(Index(-1,2), Index(1,10)))(index2 union Index2(Index(-1,0), Index(7,10)))
  }

  test("equals") {
    assert(index2==index2)
    assert(empty2==empty2)
    assert(index2!=empty2)
    assert(index2!=Index2.empty())
    assert(index2==Index2(Index(1,2), Index(1,3)))
    assert(index2!=Index2(Index(1,2), Index(1,2)))
    assert(index2!=Index2(Index(2,3), Index(2,4)))
    assert(index2!=Index2(Index(3,4), Index(4,6)))
  }

  test("iterator") {
    var k, l = 1
    for ((i, j) <- index2) {
      assertResult((k,l))((i,j))
      l += 1
      if (l>3) { k += 1; l = 1 }
    }
    for ((i, j) <- empty2) fail("executed by empty iterator")
  }

  test("apply") {
    assertResult((1,1))(index2(0))
    assertResult((1,2))(index2(1))
    assertResult((2,1))(index2(3))
  }

  test("exception") {
    intercept[IndexOutOfBoundsException](index2(6))
    intercept[IndexOutOfBoundsException](empty2(0))
  }

}