package org.garagescience.deeplearning.nosgd.linalg

import scala.collection.IterableLike

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MatrixSubtypeOwnStoreTest extends FunSuite {

  private val a1 = Matrix2x2OSt(11,12,21,22)
  private val a2 = Matrix2x2OSt(111,112,121,122)

  test("atRow is dummy on Matrix2x2OSts but yields correct type") {
    val b = a1 atRow 3
	  assertResult(1)(b.index.dim1.low)
	  assertResult(classOf[Matrix2x2OSt])(b.getClass)
  }

  test("atRow yields correct result") {
    val b = a1 atRow 3
	  assertResult(a1)(b)
  }

  test("unary ops yield correct type") {
    val b1 = +a1
    val b2 = -a1
	  assertResult(classOf[Matrix2x2OSt])(b1.getClass)
	  assertResult(classOf[Matrix2x2OSt])(b2.getClass)
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
	  assertResult(classOf[Matrix2x2OSt])(b.getClass)
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
    assertResult(Matrix2x2OSt(55,60,105,110))(b)
	  assertResult(classOf[Matrix2x2OSt])(b.getClass)
  }

  test("transposed yields correct type") {
    val b = a1.transposed()
	  assertResult(classOf[Matrix2x2OSt])(b.getClass)
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
    assertResult(Matrix2x2OSt(55,60,105,110))(b)
	  assertResult(classOf[Matrix2x2OSt])(b.getClass)
  }

  test("binary additon, subtraction and multiplication yield correct type") {
    val b1 = a1+a2
    val b2 = a1-a2
    val b3 = a1*a2
	  assertResult(classOf[Matrix2x2OSt])(b1.getClass)
	  assertResult(classOf[Matrix2x2OSt])(b2.getClass)
	  assertResult(classOf[Matrix2x2OSt])(b3.getClass)
  }

  test("binary additon, subtraction and multiplication yield correct result") {
    val b1 = a1+a2
    val b2 = a1-a2
    val b3 = a1*a2
    assertResult(Matrix2x2OSt(122,124,142,144))(b1)
    assertResult(Matrix2x2OSt(-100,-100,-100,-100))(b2)
    assertResult(Matrix2x2OSt(2673,2696,4993,5036))(b3)
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

class Matrix2x2OSt private (val a11: Int, val a12: Int, val a21: Int, val a22: Int) extends
    RowMatrix[Int](1, Seq(Vector(a11,a12), Vector(a21,a22))) with IterableLike[Vector[Int], Matrix2x2OSt] with
    MatrixLike[Int, Matrix2x2OSt] {

  val index: Index2 = Index2(Index(1,2), Index(1,2))

  def row (i: Int): Vector[Int] = i match {
    case 1 ⇒ Vector(a11,a12)
    case 2 ⇒ Vector(a21,a22)
    case _ ⇒ Vector[Int]()
  }

  def col (j: Int): Vector[Int] = j match {
    case 1 ⇒ Vector(a11,a21)
    case 2 ⇒ Vector(a12,a22)
    case _ ⇒ Vector[Int]()
  }

  def apply (i: Int, j: Int): Int = row(i)(j)

  protected val dataHashCode: Int = {
    val prime = 31
    var result = 1
    result = prime*result+Vector(a11,a12).hashCode()
    result = prime*result+Vector(a21,a22).hashCode()
    result.toInt
  }

  override protected[this] def newBuilder: MatrixBuilder[Vector[Int],Matrix2x2OSt] = Matrix2x2OSt.newBuilder

}

object Matrix2x2OSt {

  def apply (a11: Int, a12: Int, a21: Int, a22: Int) = new Matrix2x2OSt(a11,a12,a21,a22)

  def newBuilder: MatrixBuilder[Vector[Int], Matrix2x2OSt] =
    new MatrixBuilder[Vector[Int], Matrix2x2OSt] {
    	protected val zerovec = Vector[Int]()
		  def result: Matrix2x2OSt = {
				  require(elems.length==2)
				  Matrix2x2OSt(elems(0)(1),elems(0)(2),elems(1)(1),elems(1)(2))
      }
  }

  implicit def canBuildFrom: MatrixCanBuildFrom[Matrix2x2OSt, Vector[Int], Matrix2x2OSt] =
    new MatrixCanBuildFrom[Matrix2x2OSt, Vector[Int], Matrix2x2OSt] {
	    def apply (): MatrixBuilder[Vector[Int],Matrix2x2OSt] = newBuilder
      def apply (from: Matrix2x2OSt): MatrixBuilder[Vector[Int],Matrix2x2OSt] = newBuilder
  }

	implicit class ScalarOps (s: Int) {
    def * (v: Matrix2x2OSt): Matrix2x2OSt = v * s
	}

}
