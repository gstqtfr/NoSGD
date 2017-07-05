package org.garagescience.deeplearning.nosgd

import scala.collection.immutable.Seq
import org.apache.spark.ml.linalg.{Matrices, Matrix, Vector, Vectors}

// TODO: let's use a Whole Bunch of DoubleGerminalCentres here, unless there's
// TODO: a pressing need to create a new method. this'll be a Uses-A class ...

class VectorGerminalCentre(protected val v: Vector,
                           protected val popSize: Int=10,
                           protected val poolSize: Int=20) extends Hypermutate {



  // create our clonal pool (var?!)
  var clones: Seq[Vector] = for {i <- 0 until popSize} yield v

  val centres: Seq[DoubleGerminalCentre] =
    for {i <- 0 until popSize
        j <- 0 until clones(i).size}
      yield new DoubleGerminalCentre(clones(i)(j), poolSize)

  // germinal centres apply the somatic hypermutation operator to their
  // clonal pools
  def germinate: Seq[Array[Double]] = centres.map(gc => gc.germinate)

  def update(f: Vector => Double) = {
    val _clones: Seq[Vector] = germinate.map(xs => Vectors.dense(xs.toArray))
    clones = compareAndReplace(clones, _clones, f)
  }

  def compareAndReplace(l1: Seq[Vector], l2: Seq[Vector], f: Vector => Double): Seq[Vector] =
    l1.zip(l2).map { case (c, m) =>
      if (f(c) < f(m)) c else m
    }

}