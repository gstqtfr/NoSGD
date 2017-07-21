package org.garagescience.deeplearning.nosgd.bca.binary

import org.garagescience.deeplearning.nosgd.bca.{Hypermutate, SequenceSingleArgGerminalCentre}

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.ClassTag
import org.garagescience.deeplearning.nosgd.linalg._

// TODO: higher-kinded collection here? Aray[T] => S[T]?

// TODO: the clones need to move to e.g. BinarySequencePointHypermutation

class BinaryVectorGerminalCentre[T: ClassTag, U: ClassTag](val m: Array[T],
                                    override val poolSize: Int = 20)
  // TODO: not sure if it's approp. to use these traits ...
  extends Hypermutate with SequenceSingleArgGerminalCentre[Array, T, U] {

  // TODO: move to BinarySequencePointHypermutation
  var clones: Array[Array[T]] = {
    for {i <- 0 until poolSize} yield m
  }.toArray

  def germinate: Array[Array[T]] = ???
  def getClonePoolFitness(f: U => T): Array[T] = ???
  def getFittest(f: U => T): Array[T] = ???
  def update(xs1: Array[T], xs2: Array[T],xxs: Array[Array[T]]): Array[Array[T]] = ???

}
