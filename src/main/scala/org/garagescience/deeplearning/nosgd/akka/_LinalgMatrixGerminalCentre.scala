package org.garagescience.deeplearning.nosgd

import scala.language.higherKinds
import org.garagescience.deeplearning.nosgd.linalg._

// TODO: might be worth adding a B type param. here ...
trait _LinalgMatrixGerminalCentre[M[_], A] {

  def rows: Int
  def cols: Int

  val m: M[A]

  def germinate: Array[Array[A]]

  def compareAndReplace(l1: Array[M[A]],
                        l2: Array[M[A]],
                        f: M[A] => A): (Array[(M[A], A)])

  def getFittest(f: M[A] => A): Array[M[A]]

}