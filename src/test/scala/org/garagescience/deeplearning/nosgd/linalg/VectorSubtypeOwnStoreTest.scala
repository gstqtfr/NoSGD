package org.garagescience.deeplearning.nosgd.linalg

import scala.collection.IterableLike

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class VectorSubtypeOwnStoreTest extends FunSuite {

  private val p1 = PointOSt(1,2)
  private val p2 = PointOSt(3,4)

  test("@@ is dummy on Points but yields correct type") {
    val q = p1 @@ 3
	  assertResult(1)(q.index.low)
	  assertResult(classOf[PointOSt])(q.getClass)
  }

  test("@@ yields correct result") {
    val q = p1 @@ 3
	  assertResult(p1)(q)
  }

  test("unary ops yield correct type") {
    val q1 = +p1
    val q2 = -p1
	  assertResult(classOf[PointOSt])(q1.getClass)
	  assertResult(classOf[PointOSt])(q2.getClass)
  }

  test("unary ops yield correct result") {
    val q1 = +p1
    val q2 = -p1
	  assertResult(1)(q1.x)
	  assertResult(2)(q1.y)
	  assertResult(-1)(q2.x)
	  assertResult(-2)(q2.y)
  }

  test("multiplication by scalar from right yields correct type") {
    val q = p1*5
	  assertResult(classOf[PointOSt])(q.getClass)
  }

  test("multiplication by scalar from right yields correct result") {
    val q = p1*5
	  assertResult(5)(q.x)
	  assertResult(10)(q.y)
  }

  test("multiplication by scalar from left yields correct result and type") {
    val q = 5*p1
    assertResult(PointOSt(5,10))(q)
	  assertResult(classOf[PointOSt])(q.getClass)
  }

  test("map yields correct result and type") {
    val q = p1.map(_*5)
    assertResult(PointOSt(5,10))(q)
	  assertResult(classOf[PointOSt])(q.getClass)
  }

  test("binary additon and subtraction yield correct type") {
    val q1 = p1+p2
    val q2 = p1-p2
	  assertResult(classOf[PointOSt])(q1.getClass)
	  assertResult(classOf[PointOSt])(q2.getClass)
  }

  test("binary additon and subtraction yield correct result") {
    val q1 = p1+p2
    val q2 = p1-p2
	  assertResult(4)(q1.x)
	  assertResult(6)(q1.y)
	  assertResult(-2)(q2.x)
	  assertResult(-2)(q2.y)
  }

  /*
  test("norm yields correct result") {
    assert(Math.sqrt(5) ~= p1.norm)
  }
  */

  test("apply yields correct result") {
	  assertResult(1)(p1(1))
	  assertResult(2)(p1(2))
  }

}

class PointOSt private (val x: Double, val y: Double) extends
    DoubleVector(1, Seq(x,y)) with IterableLike[Double, PointOSt] with
    VectorLike[Double, PointOSt] {

	val index: Index = Index(1, 2)

  def apply (i: Int): Double = i match {
    case 1 ⇒ x
    case 2 ⇒ y
    case _ ⇒ 0.0
  }

  protected val dataHashCode: Int = {
    val prime = 31
    var result = 1
    result = prime*result+x.hashCode()
    result = prime*result+y.hashCode()
    result.toInt
  }


  override protected[this] def newBuilder: VectorBuilder[Double,PointOSt] = PointOSt.newBuilder

}

object PointOSt {

  def apply (x: Double, y: Double) = new PointOSt(x, y)

  def newBuilder: VectorBuilder[Double,PointOSt] =
    new VectorBuilder[Double, PointOSt] {
		  def result: PointOSt = {
				  require(elems.length==2)
				  PointOSt(elems(0), elems(1))
      }
  }

  implicit def canBuildFrom: VectorCanBuildFrom[PointOSt, Double, PointOSt] =
    new VectorCanBuildFrom[PointOSt, Double, PointOSt] {
	    def apply (): VectorBuilder[Double,PointOSt] = newBuilder
      def apply (from: PointOSt): VectorBuilder[Double,PointOSt] = newBuilder
  }

	implicit class ScalarOps (s: Double) {
    def * (p: PointOSt): PointOSt = p * s
	}

}
