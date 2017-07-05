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

import de.h2b.scala.lib.math._

@RunWith(classOf[JUnitRunner])
class VectorSubtypeTest extends FunSuite {

  private val p1 = Point(1,2)
  private val p2 = Point(3,4)

  test("@@ is dummy on Points but yields correct type") {
    val q = p1 @@ 3
	  assertResult(1)(q.index.low)
	  assertResult(classOf[Point])(q.getClass)
  }

  test("@@ yields correct result") {
    val q = p1 @@ 3
	  assertResult(p1)(q)
  }

  test("unary ops yield correct type") {
    val q1 = +p1
    val q2 = -p1
	  assertResult(classOf[Point])(q1.getClass)
	  assertResult(classOf[Point])(q2.getClass)
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
	  assertResult(classOf[Point])(q.getClass)
  }

  test("multiplication by scalar from right yields correct result") {
    val q = p1*5
	  assertResult(5)(q.x)
	  assertResult(10)(q.y)
  }

  test("multiplication by scalar from left yields correct result and type") {
    val q = 5*p1
    assertResult(Point(5,10))(q)
	  assertResult(classOf[Point])(q.getClass)
  }

  test("map yields correct result and type") {
    val q = p1.map(_*5)
    assertResult(Point(5,10))(q)
	  assertResult(classOf[Point])(q.getClass)
  }

  test("binary additon and subtraction yield correct type") {
    val q1 = p1+p2
    val q2 = p1-p2
	  assertResult(classOf[Point])(q1.getClass)
	  assertResult(classOf[Point])(q2.getClass)
  }

  test("binary additon and subtraction yield correct result") {
    val q1 = p1+p2
    val q2 = p1-p2
	  assertResult(4)(q1.x)
	  assertResult(6)(q1.y)
	  assertResult(-2)(q2.x)
	  assertResult(-2)(q2.y)
  }

  test("norm yields correct result") {
    assert(Math.sqrt(5) ~= p1.norm)
  }

  test("apply yields correct result") {
	  assertResult(1)(p1(1))
	  assertResult(2)(p1(2))
  }

}

class Point private (val x: Double, val y: Double) extends
    DoubleVector(1, Seq(x,y)) with IterableLike[Double, Point] with
    VectorLike[Double, Point] with SimpleVectorStore[Double] {

  override protected[this] def newBuilder: VectorBuilder[Double,Point] = Point.newBuilder

}

object Point {

  def apply (x: Double, y: Double) = new Point(x, y)

  def newBuilder: VectorBuilder[Double,Point] =
    new VectorBuilder[Double, Point] {
		  def result: Point = {
				  require(elems.length==2)
				  Point(elems(0), elems(1))
      }
  }

  implicit def canBuildFrom: VectorCanBuildFrom[Point, Double, Point] =
    new VectorCanBuildFrom[Point, Double, Point] {
	    def apply (): VectorBuilder[Double,Point] = newBuilder
      def apply (from: Point): VectorBuilder[Double,Point] = newBuilder
  }

	implicit class ScalarOps (s: Double) {
    def * (p: Point): Point = p * s
	}

}
