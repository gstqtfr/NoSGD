package org.garagescience.deeplearning.nosgd.linalg

import scala.reflect.ClassTag

import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MatrixTest extends FunSuite {

	implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(1e-6)

  private val a = Matrix(Vector(11,12,13), Vector(21,22,23))
  private val aArray = Array(Array(11,12,13), Array(21,22,23))

  private val m = 2 //number of rows of a
  private val n = 3 //number of columns of a

  private def check [E] (expected: Array[Array[E]], actual: Matrix[E], rowRange: Range, colRange: Range) = {
	  assert(rowRange.size == actual.height)
	  assert(colRange.size == actual.width)
	  for {
	    i <- rowRange
	    j <- colRange
	  } assert(expected(i-rowRange.start)(j-colRange.start) === actual(i,j))
  }

  private def check [E] (expected: Array[E], actual: Vector[E], range: Range) = {
	  assert(range.size == actual.length)
	  for (i <- range) assert(expected(i-range.start) === actual(i))
  }

  test("height and width") {
    assertResult(2)(a.height)
    assertResult(3)(a.width)
  }

  test("isSquare") {
    assert(!a.isSquare)
    assert(Matrix(Vector(1,2), Vector(3,4)).isSquare)
  }

	test("unary +") {
	  val b = +a
	  check(aArray, b, 1 to m, 1 to n)
	}

	test("unary -") {
	  val b = -a
	  check(Array(Array(-11,-12,-13), Array(-21,-22,-23)), b, 1 to m, 1 to n)
	}

	test("+") {
	  val b = Matrix(Vector(10,20,30), Vector(40,50,60)) @@ (0,1)
	  val c = a+b
	  check(Array(Array(10,20,30), Array(51,62,73), Array(21,22,23)), c, 0 to 2, 1 to 3)
	}

	test("-") {
	  val b = Matrix(Vector(10,20,30), Vector(40,50,60)) @@ (0,1)
	  val c = a-b
	  check(Array(Array(-10,-20,-30), Array(-29,-38,-47), Array(21,22,23)), c, 0 to 2, 1 to 3)
	}

	test("matrix * matrix") {
	  val b = Matrix(Vector(1,2),Vector(3,4),Vector(5,6))
	  val c = a*b
	  check(Array(Array(112,148),Array(202,268)), c, 1 to 2, 1 to 2)
	}

	test("matrix * vector") {
	  val u = Vector(1,2)
	  val v = a*u
	  check(Array(35,65), v, 1 to 2)
	}

	test("vector * matrix") {
	  val u = Vector(1,2)
	  val v = u**a
	  check(Array(53,56,59), v, 1 to 3)
	}

	test("matrix * scalar") {
	  val b = a*(-2)
	  check(Array(Array(-22,-24,-26),Array(-42,-44,-46)), b, 1 to 2, 1 to 3)
	}

	test("scalar * matrix") {
	  val b = -2*a
	  check(Array(Array(-22,-24,-26),Array(-42,-44,-46)), b, 1 to 2, 1 to 3)
	}

	test("transpose") {
	  val b = a.transposed()
	  check(Array(Array(11,21),Array(12,22),Array(13,23)), b, 1 to 3, 1 to 2)
	}

	test("row sum") {
	  val v = a.rowSum()
	  check(Array(36,66), v, 1 to 2)
	}

	test("column sum") {
	  val v = a.colSum()
	  check(Array(32,34,36), v, 1 to 3)
	}

	test("row iterator") {
	  var i = 0
	  for (row <- a) { check(aArray(i), row, 1 to 3); i += 1 }
	  val b = for (row <- a) yield row
	  assert(a === b)
	}

	test("column iterator") {
	  val colArray = Array(Array(11,21),Array(12,22),Array(13,23))
	  var j = 0
	  for (col <- a.colIterator) { check(colArray(j), col, 1 to 2); j += 1 }
	  val b = Matrix(a.colIterator.toSeq: _*)
	  assert(a.transposed === b)
	}

	test("elem iterator") {
	  val elems = Seq(11, 12, 13, 21 ,22 ,23)
	  var i = 0
		for (elem ← a.elemIterator) { assertResult(elems(i))(elem); i += 1 }
	}

	test("at row") {
	  val b = a atRow -1
	  check(aArray, b, -1 to 0, 1 to 3)
	}

	test("at column") {
	  val b = a atCol -1
	  check(aArray, b, 1 to 2, -1 to 1)
	}

	test("at all") {
	  val b = a @@ (-1,-1)
	  check(aArray, b, -1 to 0, -1 to 1)
	}

	test("shorten") {
	  val a0a = Matrix[Double]()
	  val a0b = Matrix(Vector(0,0), Vector(0,0))
    assert(a0a.shorten.isInstanceOf[Matrix[Double]])
    assert(a0b.shorten.isInstanceOf[Matrix[Int]])
	  assert(a0a.shorten.isZero)
	  assert(a0b.shorten.isZero)
	  val zerovec1 = Vector[Int]()
	  val zerovec2 = Vector(0,0)
	  val a1 = Matrix(zerovec1, zerovec2, Vector(11,12), Vector(21,22,0), zerovec1)
    assert(a1.shorten.isInstanceOf[Matrix[Int]])
	  assertResult(Matrix(Vector(11,12), Vector(21,22))@@(3,1))(a1.shorten)
	}

	test("widen") {
	  val a0 = Matrix[Int]()
	  val i0 = Index2(Index(0,1), Index(-1,1))
    assert(a0.widen(i0).isInstanceOf[Matrix[Int]])
    assertResult(Matrix(Vector(0,0,0)@@(-1), Vector(0,0,0)@@(-1)) atRow 0)(a0.widen(i0))
    val a1 = Matrix(Vector(11,12), Vector(21,22))@@(3,1)
	  val i1 = Index2(Index(1,5), Index(1,2))
    assert(a1.widen(i1).isInstanceOf[Matrix[Int]])
    val z1 = Vector(0,0)
    assertResult(Matrix(z1, z1, Vector(11,12), Vector(21,22), z1))(a1.widen(i1))
	}

	test("similariry") {
	  val zv = Vector(0)
	  val a = Matrix(Vector(11,12), zv, Vector(31,32))
	  val a1 = Matrix(zv, Vector(11,12), zv, Vector(31,32)) atRow 0
	  val a2 = Matrix(Vector(0,11,12)@@0, zv, Vector(31,32,0), zv)
	  val a3 = Matrix(zv, zv, Vector(11,12), zv, Vector(31,32), zv, zv) atRow -1
    assert(a~~a1)
    assert(a~~a2)
    assert(a~~a3)
    assert(a1~~a2)
    assert(a1~~a3)
    assert(a2~~a3)
	  val b = Matrix(Vector(11,12), Vector(21,22), Vector(31,32))
	  assert(!(a~~b))
    val a0a = Matrix[Int]()
    val a0b = Matrix[Int]()
    val a0c = Matrix[Int](Vector(0,0), Vector(0,0))
    assert(a0a~~a0b)
    assert(a0a~~a0c)
    assert(!(a~~a0a))
    assert(!(a0a~~a))
	}

	test("map") {
	  val b = a.map { row => row*2 }
	  assertResult(a.height)(b.height)
	  assertResult(a.width)(b.width)
	  check(Array(Array(22,24,26), Array(42,44,46)), b, 1 to m, 1 to n)
	}

	test("map at") {
	  val a0 = Matrix(Vector.at(0)(11,12,13), Vector.at(0)(21,22,23)) @@ (0,0)
	  val b = a0.map { row => row*2 }
	  assertResult(a0.height)(b.height)
	  assertResult(a0.width)(b.width)
	  check(Array(Array(22,24,26), Array(42,44,46)), b, 0 to 1, 0 to 2)
	}

	test("to string") {
	  assertResult("((11,12,13)@1,(21,22,23)@1)@1,1")(a.toString)
	}

	test("equals") {
	  val a1 = a
	  val a2 = Matrix(Vector(11,12,13), Vector(21,22,23))
	  val b1 = Matrix(Vector(11,12,13), Vector(21,22,23)) @@ (0,0)
	  val b2 = Matrix(Vector.at(0)(11,12,13), Vector.at(0)(21,22,23)) @@ (0,0)
	  val c = Matrix(Vector(-11,-12,-13), Vector(-21,-22,-23))
	  assert(a1 === a1)
	  assert(a1 === a2)
	  assert(a1 !== b1)
	  assert(a1 !== b2)
	  assert(a1 !== c)
	  assert(b1 == b2)
	}

}