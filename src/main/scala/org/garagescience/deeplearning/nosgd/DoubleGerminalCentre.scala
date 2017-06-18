package org.garagescience.deeplearning.nosgd

import scala.collection.immutable.Seq

// TODO: bear in mind parallelisation here ...

// TODO: PARALLELISE!!! Actor or Future. GET IT SORTED!!!

class DoubleGerminalCentre(protected val d: Double,
                           protected val poolSize: Int=20) extends Hypermutate {

  import Double2BitStringConvert._

  // create our clonal pool
  var clones: Seq[Double] = for {i <- 0 until poolSize} yield d

  // this hypermutates our clonal pool & returns it
  /**
    * germinate hypermutates the clonal pool
    * @return mutated clones of the original clonal pool
    */
  def germinate: Seq[Double] = clones.map(clone => somaticHypermutation(new StringBuffer(toBinaryString(clone))))

  /**
    * Updates the population of clones according to the fitness/error function
    * @param f fitness/error/loss function
    */
  def update(f: Double => Double) = {
    // this gets us a mutated version of our clones
    val _clones = germinate
    clones = compareAndReplace(clones, _clones, f)
  }

  /**
    * cmpAndReplace takes a pair of clone sequences & returns the fittest member by
    * comparing the elements with the passed function f.
    * @param l1 List of clones (usually the clones sequence above)
    * @param l2 List of (mutated) clones
    * @param f Fitness/error/loss function
    * @return The fittest clones from the two lists according to the function f
    */
  def compareAndReplace(l1: Seq[Double], l2: Seq[Double], f: Double => Double): Seq[Double] =
    l1.zip(l2).map { case (c, m) =>
      if (f(c) < f(m)) c else m
  }

}