package org.garagescience.deeplearning.nosgd.bca

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.collection.immutable.{Seq=>TSeq}

class FixedPointHypermutation(val m: TSeq[Double],
                              override val poolSize: Int = 10)
  extends Hypermutate with SequenceSingleArgGerminalCentre[TSeq, Double, Int] {

  var clones: TSeq[TSeq[Double]] = { for {i <- 0 until poolSize} yield m }

  val centres: TSeq[GerminalCentre] = {
    // FIXME: the ONLY difference between these two is the TYPE OF THE CENTRE!!
    // FIXME: this is trivial to make more generic/type param'd
    for {i <- 0 until m.length} yield new FixedPointGerminalCentre(m(i), poolSize)
  }

  def update(currentFitness: TSeq[Double],
             newFitness: TSeq[Double],
             newClones: TSeq[TSeq[Double]]): TSeq[TSeq[Double]] =
    (0 until clones.length).map { idx =>
      if (newFitness(idx) <= currentFitness(idx)) newClones(idx) else clones(idx)
    }



  override def germinate: TSeq[TSeq[Double]] =
    centres.
      map(gc => gc.germinate).
      transpose


  override def getFittest(f: Int => Double): TSeq[Double] = {
    val poolFitness = getClonePoolFitness(f)
    val fitnessMap = poolFitness.zipWithIndex.map { case (u, v) => (v, u) }.toMap
    import scala.collection.immutable.ListMap
    val fittestIndex = ListMap(fitnessMap.toSeq.sortWith(_._2 < _._2): _*).keys.head
    clones(fittestIndex)
  }


  // applies the fitness function f to each of the clones, returns their
  // fitness
  override def getClonePoolFitness(f: Int => Double): TSeq[Double] = clones.
    zipWithIndex.
    map { case (clone, idx) => f(idx) }




}