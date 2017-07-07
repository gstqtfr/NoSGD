package org.garagescience.deeplearning.nosgd

import scala.language.higherKinds

trait IntegralGerminalCentre[A, B] {

  def germinate: Array[A]

  def update(f: A => B)

  def compareAndReplace(l1: Array[A], l2: Array[A], f: A => B): Array[B]

}