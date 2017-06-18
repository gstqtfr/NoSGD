package org.garagescience.deeplearning.nosgd

import scala.collection.immutable.Seq
import org.apache.spark.ml.linalg.{Matrices, Matrix, Vector, Vectors}

// TODO: this has *got* to be type-parameterised!!!
// TODO: I'M NOT FUCKING KIDDING!!! this MUST BE PARAMETERISED!!!

class MatrixGerminalCentre(protected val m: Matrix,
                           protected val popSize: Int = 10,
                           protected val poolSize: Int = 20) extends Hypermutate {

  import Matrix2BinarySeq._

  val rows = m.numRows
  val cols = m.numCols

  // create our clonal pool (var?!)
  var clones: Seq[Matrix] = for {i <- 0 until popSize} yield m

  // TODO: so ... how do we build this for a Matrix?
  // initialise our germinal centres
  val centres: Seq[DoubleGerminalCentre] = for {i <- 0 until popSize
                     row <- 0 until rows
                     col <- 0 until cols}
    yield new DoubleGerminalCentre(m(row, col), rows * cols)

  // germinal centres apply the somatic hypermutation operator to their
  // clonal pools

  protected def germinate: Seq[Seq[Double]] = centres.map(gc => gc.germinate)

  protected def compareAndReplace(l1: Seq[Matrix],
                                  l2: Seq[Matrix],
                                  f: Matrix => Double): Seq[Matrix] =
    l1.zip(l2).map { case (c, m) =>
      if (f(c) < f(m)) c else m
    }

  // TODO: need to look out for:
  // TODO: java.lang.IllegalArgumentException
  // TODO: on Matrix dims ...
  def update(f: Matrix => Double) = {
    val _clones: Seq[Matrix] = germinate.map(xs => Matrices.dense(rows, cols, xs.toArray))
    clones = compareAndReplace(clones, _clones, f)
  }

}