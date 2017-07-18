package org.garagescience.deeplearning.nosgd.bca

import scala.language.higherKinds
import scala.language.implicitConversions

// T is usu. (by default?) going to be Double

trait SequenceSingleArgGerminalCentre[M[_], T, U] {

  def poolSize: Int

  def germinate: M[M[T]]

  def getFittest(f: U => T): M[T]

  def getClonePoolFitness(f: U => T): M[T]

  def update(xs1: M[T], xs2: M[T], xxs: M[M[T]]): M[M[T]]

}

