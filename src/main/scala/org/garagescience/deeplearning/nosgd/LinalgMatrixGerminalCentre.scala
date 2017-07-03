package org.garagescience.deeplearning.nosgd

import scala.collection.immutable.Seq
import org.apache.spark.ml.linalg.{Matrices, Matrix, Vector, Vectors}
import org.garagescience.deeplearning.nosgd.linalg._

// TODO: this has *got* to be type-parameterised!!!
// TODO: I'M NOT FUCKING KIDDING!!! this MUST BE PARAMETERISED!!!

// TODO: WHAT WE NEED is to type-parameterise using HIGHER KINDED TYPES!!!

// TODO: one way of doing this would be to have a Dimensions param here, since
// TODO: about the only problem would be with the rows & columns - this is
// TODO: *CLEARLY* a tuple, e.g. Tuple2!!!

// TODO: this is a nice idea. but bollox to it, it'll take too long just now ...

class LinalgMatrixGerminalCentre(protected val m: _Matrix,
                                 //protected val popSize: Int = 10,
                                 protected val poolSize: Int = 20) extends Hypermutate {

  import Matrix2BinarySeq._

  val rows = m.numRows
  val cols = m.numCols

  // create our clonal pool (var?!)
  var clones: Seq[_Matrix] = for {i <- 0 until poolSize} yield m

  // initialise our germinal centres
  val centres: Seq[DoubleGerminalCentre] = for {i <- 0 until poolSize
                     row <- 0 until rows
                     col <- 0 until cols}
    yield new DoubleGerminalCentre(m(row, col), rows * cols)

  // germinal centres apply the somatic hypermutation operator to their
  // clonal pools

  protected def germinate: Seq[Seq[Double]] = centres.map(gc => gc.germinate)

  protected def compareAndReplace(l1: Seq[_Matrix],
                                  l2: Seq[_Matrix],
                                  f: _Matrix => Double): Seq[(_Matrix, Double)] =
    l1.zip(l2).map { case (c, m) =>
      val f_of_c = f(c)
      val f_of_m = f(m)
      if (f(c) < f(m)) (c, f_of_c) else (m, f_of_m)
    }

  def getFittest(f: _Matrix => Double): Seq[_Matrix] = {
    clones.sortWith { case (a,b) => f(a) < f(b) }
  }

  // TODO: need to look out for:
  // TODO: java.lang.IllegalArgumentException
  // TODO: on Matrix dims ...
  def update(f: _Matrix => Double): Seq[Double] = {
    val _clones: Seq[_Matrix] = germinate.map(xs => new _DenseMatrix(rows, cols, xs.toArray))
    val clonesAndFitness: Seq[(_Matrix, Double)] = compareAndReplace(clones, _clones, f)
    clones = clonesAndFitness.map { case (c,f) => c }
    clonesAndFitness.map { case (c,f) => f }
  }

}