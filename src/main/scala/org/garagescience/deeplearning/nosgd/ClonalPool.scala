package org.garagescience.deeplearning.nosgd

import org.apache.spark.ml.linalg.{Vector, Vectors}
import org.apache.spark.ml.linalg.{Matrices, Matrix}

import scala.collection.immutable.Seq

trait _MLPClonalPool {

  def poolSize: Int

  def cloneLayer(xsw: Seq[Seq[Double]],
                 xsb: Seq[Double]): (Seq[Seq[Seq[Double]]], Seq[Seq[Double]])

  def cloneWeights(xs: Seq[Seq[Double]]): Seq[Seq[Seq[Double]]]

  def cloneBiases(xs: Seq[Double]): Seq[Seq[Double]]

}

// TODO: huh ... er ... dunno ...

// TODO: okay. this is taking a while, & i'm getting fed up with my own
// TODO: lack of concentration here ... so ... let's think

// TODO: shall i create a SomHype class *just* for Matrices, Vectors?
// TODO: because i've been doing a *lot* of buggering about here &
// TODO: not getting very far ...

class MLPClonalPool(override val poolSize: Int) extends _MLPClonalPool {

  override def cloneWeights(xs: Seq[Seq[Double]]): Seq[Seq[Seq[Double]]] =
    {for {i <- 0 until poolSize} yield xs}

  override def cloneBiases(xs: Seq[Double]): Seq[Seq[Double]] =
    {for {i <- 0 until poolSize} yield xs}

  override def cloneLayer(xsw: Seq[Seq[Double]],
                          xsb: Seq[Double]): (Seq[Seq[Seq[Double]]], Seq[Seq[Double]]) =
    (cloneWeights(xsw), cloneBiases(xsb))





}