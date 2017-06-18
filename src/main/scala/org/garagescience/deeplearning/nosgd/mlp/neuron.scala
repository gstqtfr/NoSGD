package org.garagescience.deeplearning.nosgd.mlp

import org.apache.spark.ml.linalg.{Vector, Vectors}


/**
  * Provides the activation functions contained in a neuron
  */
object Neuron {

  def sigmoid(x: Vector): Vector =
    Vectors.dense( x.toArray.map { elem => 1.0 / (1.0 + Math.exp(-elem)) }  )

  def sigmoidPrime(x: Vector): Vector =
    Vectors.dense(x.toArray map { elem =>
      val sig = 1.0 / (1.0 + Math.exp(-elem))
      sig * (1.0 - sig)
    })

  def identity(x: Vector): Vector = x.copy

  def identityPrime(x: Vector) =
    Vectors.dense((for (i <- 0 to x.size) yield 1.0).toArray)

}
