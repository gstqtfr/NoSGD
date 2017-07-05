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

import java.util.Date

import scala.collection.immutable.{ Map => ImmutableMap }
import scala.collection.mutable.{ Map => MutableMap }

import de.h2b.scala.lib.math.stat.Uniform
import de.h2b.scala.lib.phys.Time
import de.h2b.scala.lib.util.PerformanceMeter._
import de.h2b.scala.lib.util.PerformanceMeter

/**
 * Executes performance checks of various LinAlg operations.
 *
 * @author h2b
 */
object PerformanceCheck {

  private val Tasks = Seq(
      vectorCreate,
      vectorUnaryPlus, vectorUnaryMinus,
      vectorPlus, vectorMinus,
      vectorScalarProduct, vectorTimesScalar, vectorNorm,
      matrixCreate,
      matrixUnaryPlus, matrixUnaryMinus,
      matrixPlus, matrixMinus,
      matrixTimesMatrix, matrixTimesVector, matrixTimesScalar,
      matrixTransposed, matrixRowSum, matrixColSum
		)

  private final val N = 1000
  private final val M = 100
  private final val Repeats = 500

  private lazy val uniform = Uniform() //lazy to get initialized in task objects

  def main(args: Array[String]): Unit = {
	  println(s"""LinAlg -- Performance checks
               |
               |Date: ${new Date()}
  	           |
               |N = $N
               |M = $M
               |Repeats = $Repeats
             """.stripMargin)
	  val totals = PerformanceMeter.measurements(Tasks, Repeats)
	  println("\n                task \t         time/ns\n")
	  for (total ← totals.toList.sortBy(_._1.toString))
	    printf("%20s \t %15.3f\n", total._1, total._2/Repeats.toDouble)
	  val sum = totals.values.sum
	  printf("\n%20s \t %15d (%s)\n", "sum", sum, Time(sum/1000000).toString())
  }

  object vectorCreate extends Task {
    def perform () = randomVector(N)
  }

  object vectorUnaryPlus extends Task {
    val v = randomVector(N)
    def perform () = +v
  }

  object vectorUnaryMinus extends Task {
    val v = randomVector(N)
    def perform () = -v
  }

  object vectorPlus extends Task {
    val u = randomVector(N)
    val v = randomVector(N)
    def perform () = u+v
  }

  object vectorMinus extends Task {
    val u = randomVector(N)
    val v = randomVector(N)
    def perform () = u-v
  }

  object vectorScalarProduct extends Task {
    val u = randomVector(N)
    val v = randomVector(N)
    def perform () = u*v
  }

  object vectorTimesScalar extends Task {
    val v = randomVector(N)
    val s = randomScalar()
    def perform () = v*s
  }

  object vectorNorm extends Task {
    val v = randomVector(N)
    def perform () = v.norm
  }

  object matrixCreate extends Task {
	  def perform () = randomMatrix(M, N)
  }

  object matrixUnaryPlus extends Task {
    val a = randomMatrix(M, N)
    def perform () = +a
  }

  object matrixUnaryMinus extends Task {
    val a = randomMatrix(M, N)
    def perform () = -a
  }

  object matrixPlus extends Task {
    val a = randomMatrix(M, N)
    val b = randomMatrix(M, N)
    def perform () = a+b
  }

  object matrixMinus extends Task {
    val a = randomMatrix(M, N)
    val b = randomMatrix(M, N)
    def perform () = a-b
  }

  object matrixTimesMatrix extends Task {
    val a = randomMatrix(M, N)
    val b = randomMatrix(M, N)
    def perform () = a*b
  }

  object matrixTimesVector extends Task {
    val a = randomMatrix(M, N)
    val v = randomVector(N)
    def perform () = a*v
  }

  object matrixTimesScalar extends Task {
    val a = randomMatrix(M, N)
    val s = randomScalar()
    def perform () = a*s
  }

  object matrixTransposed extends Task {
    val a = randomMatrix(M, N)
    def perform () = a.transposed()
  }

  object matrixRowSum extends Task {
    val a = randomMatrix(M, N)
    def perform () = a.rowSum()
  }

  object matrixColSum extends Task {
    val a = randomMatrix(M, N)
    def perform () = a.colSum()
  }

  def randomVector (n: Int) = Vector((_: Int) ⇒ uniform.next(), 1, n)

  def randomMatrix (m: Int, n: Int) = Matrix((_: Int) ⇒ randomVector(n), 1, m)

  def randomScalar () = uniform.next()

}