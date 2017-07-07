package org.garagescience.deeplearning.nosgd.mlp

import org.apache.spark.mllib.linalg.{Vector, Vectors}
import org.apache.spark.mllib.linalg.{Matrices, Matrix}
// import org.garagescience.deeplearning.nosgd.LayerElements

import scala.collection.immutable.{Seq => ISeq}

class SomHypeNetwork(sizes: Array[Int],
                     private val learningRate: Double=0.01) extends MLPNetwork {

  var layers: Array[Layer] = sizes.zipWithIndex map { case (elem, idx) =>
    idx match {
      case 0                              =>
        new Layer(elem, sizes(idx + 1), learningRate, inputLayer=true)
      case i if (idx == sizes.length - 1) =>
        new Layer(elem, learningRate=learningRate, outputLayer=true)
      case _                              =>
        new Layer(elem, sizes(idx + 1), learningRate)
    }
  }




}