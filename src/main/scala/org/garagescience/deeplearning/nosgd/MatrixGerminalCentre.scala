package org.garagescience.deeplearning.nosgd

import scala.collection.immutable.{Seq => TSeq}
import org.garagescience.deeplearning.nosgd.linalg._


// TODO: param. on Double here?!!?

class MatrixGerminalCentre(override val m: Matrix[Double],
                           override val poolSize: Int = 20)
  extends Hypermutate with SequenceGerminalCentre[Matrix, Double, Double] {

  override val rows = m.height
  override val cols = m.width

  // create our clonal pool (var?!)
  var clones: Array[Matrix[Double]] = {
    for {i <- 0 until poolSize} yield m
  }.toArray

  // initialise our germinal centres
  // TODO: need to think about this - going to really muck up type-param on this ...
  // TODO: the thing to do here is to create a trait for DoubleGerminalCentre
  // TODO: then we can pass the trait as a param & then type param this ...
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

  override def getClonePoolFitness(f: Matrix[Double] => Double): Array[Double] =
    clones.map(xs => f(xs))


  def fromArray(rows: Int, xs: Array[Double]): Array[Vector[Double]] =
    xs.grouped(cols).toArray.map(a => Vector(a: _*))


  // TODO: need to look out for:
  // TODO: java.lang.IllegalArgumentException
  // TODO: on Matrix dims ...
  override def update(f: Matrix[Double] => Double): Array[Double] = {
    val _clones: Array[Matrix[Double]] =
    // TODO: fromArray(3)?!? shurely shome mishtake!!!??!?!
      germinate.map((xs: Array[Double]) => Matrix.atRow(0)(fromArray(3, xs): _*))
    val clonesAndFitness = compareAndReplace(clones, _clones, f)
    clones = clonesAndFitness.map { case (c, f) => c }
    clonesAndFitness.map { case (c, f) => f }
  }

}