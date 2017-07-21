package org.garagescience.deeplearning.nosgd.bca

import scala.language.higherKinds

trait IntegralGerminalCentre[S[_], A, B] {

  def germinate: S[A]

  def update(f: A => B)

  def compareAndReplace(l1: S[A], l2: S[A], f: A => B): S[B]

}