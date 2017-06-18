package org.garagescience.deeplearning.nosgd.mlp.fbca

import org.garagescience.deeplearning.nosgd.Double2BitStringConvert._
import org.garagescience.deeplearning.nosgd.Hypermutate

import scala.collection.immutable.Seq

// TODO: the ONLY difference between these guys (Single/Sequence) is that
// TODO: we're passing a sequence to the evaluation & germinate functions
// TODO: this needs sorting ...

// TODO: can I get this to work with type-params?!

object SequenceMutation extends Hypermutate {

  def germinate(sos: Seq[Double])(f: Seq[Double] => Double)(loop: Int = 50): Seq[Double] = {

    def _germinate(index: Int, xs: Seq[Double], poolSize: Int = 20): Seq[Double] = index match {

      case idx: Int if idx > 0 =>
        // we keep the original in case none of our mutants are better
        val ssb: Seq[StringBuffer] = xs.map(d => new StringBuffer(toBinaryString(d)))
        val xs1: Seq[Seq[Double]] = ssb.map(sb => clonalPool(sb)(poolSize))
        val xs2 = xs1.map(_xs => evaluate(_xs)(f)(true))
        _germinate(index - 1, xs2)

      case idx: Int if idx == 0 => xs

    }

    _germinate(loop, sos)
  }

  protected def evaluate(xs: Seq[Double])
                        (f: Seq[Double] => Double)
                        (minimise: Boolean = true): Double = {

    // apply the loss/objective/error function
    val perf: Seq[(Double, Double)] = xs.map(d => (f(xs), d))

    // sort it, so that we're maximising or minimising
    val s3 = if (minimise)
      perf.sortBy(x => x._1)
    else
      perf.sortBy(x => x._1).reverse

    // return the best candidate from the clonal pool
    s3.head._2

  }

}


object SequenceMutationNoEval extends Hypermutate {

  // TODO: think about how to add the "loser gets nuked by random seq" operator
  def germinate(sos: Seq[Double], poolSize: Int = 20): Seq[Seq[Double]] = {

    val ssb: Seq[StringBuffer] = sos.map(d => new StringBuffer(toBinaryString(d)))
    // we keep the original in case none of our mutants are better
    ssb.map(sb => clonalPool(sb)(poolSize)) :+ sos
  }
}