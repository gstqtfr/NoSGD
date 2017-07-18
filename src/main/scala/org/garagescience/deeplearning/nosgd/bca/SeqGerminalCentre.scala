package org.garagescience.deeplearning.nosgd.bca

import scala.language.higherKinds

// TODO: might be worth adding a B type param. here ...
trait SeqGerminalCentre[M[_], A, B] {

  def poolSize: Int

  val m: M[A]

  def germinate: M[M[A]]

  def compareAndReplace(l1: Array[M[A]],
                        l2: Array[M[A]],
                        f: M[A] => B): (Array[(M[B], B)])

  def getFittest(f: M[A] => B): Array[M[B]]

  def update(f: M[A] => B): Array[B]

  def getClonePoolFitness(f: A => B): Array[B]

}
