package org.garagescience.deeplearning.nosgd.bca.binary

import org.garagescience.deeplearning.nosgd.bca.{Hypermutate, SequenceSingleArgGerminalCentre}

import scala.language.higherKinds
import scala.language.implicitConversions
import org.garagescience.deeplearning.nosgd.linalg._

class BinaryVectorGerminalCentre[T](val m: Array[T],
                                    override val poolSize: Int = 20)
  extends Hypermutate with SequenceSingleArgGerminalCentre[Array, T, Int] {

  var clones: Array[Array[T]] = {
    for {i <- 0 until poolSize} yield m
  }.toArray

  def germinate: Array[Array[T]] = ???
  def getClonePoolFitness(f: Int => T): Array[T] = ???
  def getFittest(f: Int => T): Array[T] = ???
  def update(xs1: Array[T], xs2: Array[T],xxs: Array[Array[T]]): Array[Array[T]] = ???

}
