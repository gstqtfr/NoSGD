package org.garagescience.deeplearning.nosgd.linalg

import scala.reflect.{ ClassTag, classTag }

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ZeroVectorTest extends FunSuite {

  testForType(Vector[Double](), Vector.scal0[Double], Vector[Double](1,2,3), 2.0)
  testForType(Vector[Int](), Vector.scal0[Int], Vector[Int](1,2,3), 2)
  testForType(Vector[Long](), Vector.scal0[Long], Vector[Long](1,2,3), 2L)

  def testForType [A : ClassTag] (zerovec: Vector[A], zeroscal: A, othervec: Vector[A], otherscal: A) {

    val typename = "zerovec of " + classTag[A].runtimeClass.getSimpleName

	  test(s"$typename: length is 0") {
		  assertResult(0)(zerovec.length)
	  }

    test(s"$typename: index is empty") {
    	assert(zerovec.index.isEmpty)
    }

    test(s"$typename: is zero") {
    	assert(zerovec.isZero)
    	assert(!othervec.isZero)
    }

    test(s"$typename: applying yields scal0") {
      assertResult(zeroscal)(zerovec(1))
    }

    test(s"$typename: unary plus/minus yields zerovec") {
      assertResult(zerovec)(+zerovec)
      assertResult(zerovec)(-zerovec)
    }

    test(s"$typename: addition and subtraction is neutral") {
      assertResult(zerovec)(zerovec+zerovec)
      assertResult(zerovec)(zerovec-zerovec)
      assertResult(othervec)(othervec+zerovec)
      assertResult(othervec)(zerovec+othervec)
      assertResult(othervec)(othervec-zerovec)
      assertResult(-othervec)(zerovec-othervec)
    }

    test(s"$typename: scalar product is zero") {
      assertResult(0)(zerovec*zerovec)
      assertResult(0)(zerovec*othervec)
      assertResult(0)(othervec*zerovec)
    }

    test(s"$typename: scaling yields zerovec") {
      assertResult(zerovec)(zerovec*otherscal)
    }

    test(s"$typename: norm is zero") {
      assertResult(0.0)(zerovec.norm)
    }

    test(s"$typename: iterator is empty") {
      for (elem ← zerovec) fail("executed by empty iterator")
    }

    test(s"$typename: at yields zero vector") {
      assert((zerovec @@ -1).isZero)
      //TODO: put this into docu:
      /* This is the first reason to have isZero: zerovecs with different index
       * ranges are not equal. We could remedy this by requiring all zerovecs
       * to have the same index range (e.g., [+Maxdex, -Maxdex]) which could be
       * achieved by adjusting low and high in Index if it is empty, but this
       * migtht have unwanted side effects (e.g., Vector.at(1)() would
       * likely not have the explicit specified lower bound).
       */
    }

    test(s"$typename: string representation is okay") {
      assertResult("()@1")(zerovec.toString)
    }

    test(s"$typename: equals is consistent") {
    	assert(zerovec==zerovec)
    	assert(zerovec!=othervec)
    	assert(othervec!=zerovec)
    }

  }

  test(s"All: different zero-vector objects of the same type and lower index are equal") {
	  assertResult(Vector[Double]())(Vector[Double]())
	  assertResult(Vector[Int]())(Vector[Int]())
	  assertResult(Vector[Long]())(Vector[Long]())
  }

  test("All: map function yields zerovec") {
	  assertResult(Vector[Double]())(Vector[Double]().map(_*2))
	  assertResult(Vector[Int]())(Vector[Int]().map(_*2))
	  assertResult(Vector[Long]())(Vector[Long]().map(_*2))
  }

}