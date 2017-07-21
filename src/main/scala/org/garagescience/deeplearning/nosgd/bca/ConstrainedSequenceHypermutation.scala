package org.garagescience.deeplearning.nosgd.bca

import scala.language.higherKinds
import scala.language.implicitConversions

// TODO: replace the weakest clone (or maybe population member?) with new
// TODO: randomised genetic material; shake things up!

class ConstrainedSequenceHypermutation(val m: Array[Double],
                                       override val poolSize: Int = 20)
  extends Hypermutate with SequenceSingleArgGerminalCentre[Array, Double, Int] {

  var clones: Array[Array[Double]] = {
    for {i <- 0 until poolSize} yield m
  }.toArray

  val centres: Array[GerminalCentre] = {
    for {i <- 0 until m.length} yield new ConstrainedGerminalCentre(m(i), 2.0, poolSize)
  }.toArray


  // so. we can germinate new hypermutated clones.
  // we can get their fitness, using the function f.
  // we can get the fittest clone.
  // now we need to do update, so that if
  // the clone is fitter than the original, we replace
  // it. er, that's it ...


  def update(currentFitness: Array[Double],
             newFitness: Array[Double],
             newClones: Array[Array[Double]]): Array[Array[Double]] =
    (0 until clones.length).map { idx =>
      if (newFitness(idx) <= currentFitness(idx)) newClones(idx) else clones(idx)
    }.toArray



  override def germinate: Array[Array[Double]] =
    centres.
      map(gc => gc.germinate).
      transpose


  override def getFittest(f: Int => Double): Array[Double] = {
    val poolFitness = getClonePoolFitness(f)
    val fitnessMap = poolFitness.zipWithIndex.map { case (u, v) => (v, u) }.toMap
    import scala.collection.immutable.ListMap
    // TODO: see if we want to embiggen the score or not

    // TODO: this is to embiggen the scores:
    // TODO: val fittestIndex = ListMap(fitnessMap.toSeq.sortWith(_._2 > _._2): _*)
    // TODO: this is to ensmallify:
    // TODO: val fittestIndex = ListMap(fitnessMap.toSeq.sortWith(_._2 < _._2): _*)
    // TODO: & this:
    // TODO: val fittestIndex = ListMap(fitnessMap.toSeq.sortBy(_._2): _*).keys.head
    // val fittestIndex = ListMap(fitnessMap.toSeq.sortBy(_._2): _*).keys.head
    val fittestIndex = ListMap(fitnessMap.toSeq.sortWith(_._2 > _._2): _*).keys.head
    clones(fittestIndex)
  }


  // applies the fitness function f to each of the clones, returns their
  // fitness
  override def getClonePoolFitness(f: Int => Double): Array[Double] =
  clones.
    zipWithIndex.
    map { case (clone, idx) => f(idx) }



}