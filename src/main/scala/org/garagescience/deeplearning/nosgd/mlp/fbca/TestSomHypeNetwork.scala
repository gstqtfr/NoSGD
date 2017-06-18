package org.garagescience.deeplearning.nosgd.mlp.fbca

import org.apache.spark.ml.linalg.Vectors

// this solves the function:
// f(x) = x^3 - 2x^2 + 4
// BUT! none of that pricking around with gradient descent or
// backprop. nope! somhype all the way ...


// this is a book-keeping class to keep track of which weights
// & biases go where in this layer
final case class LayerElements(wRows: Int,
                               wCols: Int,
                               bLength: Int)


object TestSomHypeNeuralNet1 {

  import org.garagescience.deeplearning.nosgd.mlp._
  import org.garagescience.deeplearning.nosgd.Matrix2BinarySeq._
  import org.garagescience.deeplearning.nosgd.Vector2BinarySeq._

  // TODO: actor this baby up a bit ...
  def main(args: Array[String]): Unit = {

    // actually, the learning rate is irrelevant here ...
    // TODO: learningRate - get rid!!!
    val n = new SomHypeNetwork(Array(1, 3, 1), learningRate = 0.001)

    // TODO: hmm ...
    // val weights = n.getWeights.toSeq
    // val biases  = n.getBiases.toSeq

    // val decon: List[LayerElements] = deconstructNetwork(n)


    // TODO: we download the weights & biases from the MLP network.
    val sxw: List[Seq[Seq[Double]]] = n.getWeights.map(m => m.toSeqOfSeq).toList
    val xsb = n.getBiases.map(b => b.toSeq)

    // TODO: so ... the idea here is to create a clonal pool.



    // TODO: clonal algorithm, then get the performance



    // val thingfish = sxw.map(m => m.toSeqOfSeq)








    val feats = (for (i <- -50 to 49) yield Vectors.dense(i / 100.0)).toArray
    val targs = (for (i <- -50 to 49)
      yield Vectors.dense((Math.pow(i, 3) - (2 * Math.pow(i, 2)) + 4) / 10000.0)).toArray



    // TODO: we mutate.
    // TODO: we upload it back to the net
    // TODO: we evaluate the performance
    // TODO: then we seal that into the network until next iteration ...

    // n.fit(feats, targs, n.updateWeights, iterations=1000)

    feats zip targs foreach { case (feature, target) =>
      val predicted = n.predict(feature)
      val point = predicted.apply(0) * 10000
      val d: Double = target.apply(0)
      println(s"$d,$point")
    }

  }

}