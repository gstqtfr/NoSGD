package org.garagescience.deeplearning.nosgd

import scala.language.higherKinds

// TODO: might be worth adding a B type param. here ...
trait SequenceGerminalCentre[M[_], A, B] {

  def rows: Int
  def cols: Int
  def poolSize: Int

  val m: M[A]

  def germinate: Array[Array[A]]

  def compareAndReplace(l1: Array[M[A]],
                        l2: Array[M[A]],
                        f: M[A] => B): (Array[(M[B], B)])

  def getFittest(f: M[A] => B): Array[M[B]]

  def update(f: M[A] => B): Array[B]

  // gc.clones.map(xs => error(xs))
  def getClonePoolFitness(f: M[A] => B): Array[B]

}