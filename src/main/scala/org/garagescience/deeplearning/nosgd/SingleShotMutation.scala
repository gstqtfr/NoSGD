package org.garagescience.deeplearning.nosgd

import scala.collection.immutable.{IndexedSeq, Seq}
import Double2BitStringConvert._

/**
  * TODO: we need Futures or Actors here. the BCA is a prime candidate
  * TODO: for parallelisation ...
  */


object SingleShotMutation extends Hypermutate {

  // TODO: single shot germinate? rather than iterating, just recurse over
  // TODO: the sequence

  def germinate(sos: Seq[Double])(f: Double => Double)(loop: Int = 50): Seq[Double] = {

    def _germinate(index: Int, xs: Seq[Double], poolSize: Int = 20): Seq[Double] = index match {

      case idx: Int if idx > 0 =>
        // we keep the original in case none of our mutants are better
        val ssb: Seq[StringBuffer] = xs.map(d => new StringBuffer(toBinaryString(d)))
        val xs1: Seq[Seq[Double]] = ssb.map(sb => clonalPool(sb)(poolSize))
        val xs2 = xs1.map(_xs => evaluate(_xs)(f)(true))
        _germinate(index-1, xs2)

      case idx: Int if idx == 0 => xs

    }

    _germinate(loop, sos)
  }

  protected def evaluate(xs: Seq[Double])
                        (f: Double => Double)
                        (minimise: Boolean = true): Double = {

    // apply the loss/objective/error function
    val perf: Seq[(Double, Double)] = xs.map(d => (f(d), d))

    // sort it, so that we're maximising or minimising
    val s3 = if (minimise)
      perf.sortBy(x => x._1)
    else
      perf.sortBy(x => x._1).reverse

    // return the best candidate from the clonal pool
    s3.head._2
  }
}
