package org.garagescience.deeplearning.nosgd

import scala.collection.immutable.{Seq => TSeq}
import org.garagescience.deeplearning.nosgd.linalg._

// TODO: this has *got* to be type-parameterised!!!
// TODO: I'M NOT FUCKING KIDDING!!! this MUST BE PARAMETERISED!!!

// TODO: WHAT WE NEED is to type-parameterise using HIGHER KINDED TYPES!!!

// TODO: one way of doing this would be to have a Dimensions param here, since
// TODO: about the only problem would be with the rows & columns - this is
// TODO: *CLEARLY* a tuple, e.g. Tuple2!!!

// TODO: this is a nice idea. but bollox to it, it'll take too long just now ...

// can now type-param quite easily ...

class LinalgMatrixGerminalCentre(override val m: Matrix[Double],
                                 //protected val popSize: Int = 10,
                                 val poolSize: Int = 20)
  extends Hypermutate with _LinalgMatrixGerminalCentre[Matrix, Double] {

  //import Matrix2BinarySeq._

  val rows = m.height
  val cols = m.width

  // create our clonal pool (var?!)
  var clones: Array[Matrix[Double]] = {
    for {i <- 0 until poolSize} yield m
  }.toArray

  // initialise our germinal centres
  val centres: TSeq[DoubleGerminalCentre] = for {i <- 0 until poolSize
                                                 row <- 0 until rows
                                                 col <- 0 until cols}
    yield new DoubleGerminalCentre(m(row, col), rows * cols)

  // germinal centres apply the somatic hypermutation operator to their
  // clonal pools
  override def germinate: Array[Array[Double]] = centres.map(gc => gc.germinate).toArray

  override def compareAndReplace(l1: Array[Matrix[Double]],
                                 l2: Array[Matrix[Double]],
                                 f: Matrix[Double] => Double): Array[(Matrix[Double], Double)] =
    l1.zip(l2).map { case (c, m) =>
      val f_of_c = f(c)
      val f_of_m = f(m)
      if (f(c) < f(m)) (c, f_of_c) else (m, f_of_m)
    }

  override def getFittest(f: Matrix[Double] => Double): Array[Matrix[Double]] =
    clones.sortWith { case (a, b) => f(a) < f(b) }


  def fromArray(rows: Int, xs: Array[Double]): Array[Vector[Double]] =
    xs.grouped(cols).toArray.map(a => Vector(a: _*))


  // TODO: need to look out for:
  // TODO: java.lang.IllegalArgumentException
  // TODO: on Matrix dims ...
  def update(f: Matrix[Double] => Double): Array[Double] = {
    val _clones: Array[Matrix[Double]] =
    // TODO: right, let's get this sorted ...
      germinate.map((xs: Array[Double]) => Matrix.atRow(0)(fromArray(3, xs): _*))

    val clonesAndFitness = compareAndReplace(clones, _clones, f)
    clones = clonesAndFitness.map { case (c, f) => c }
    clonesAndFitness.map { case (c, f) => f }
  }

}