package org.garagescience.deeplearning.nosgd.mlp.fbca

object NetworkDeconstructor {

  /*
  def deconstructLayer(layer: Layer): LayerElements = {
    val (weights: Matrix, biases: Vector) = layer.get
    LayerElements(weights.numRows, weights.numCols, biases.size)
  }

  def deconstructNetwork(n: MLPNetwork) = n.layers.map(l => deconstructLayer(l)).toList

  // TODO: right. we need to reconstruct our layer. we have LayerElements to
  // TODO: tell us how many rows & columns we have, & the size of the bias
  // TODO: vector. so, we just iterate over the sequence, assigning in the
  // TODO: same order as we downloaded them.

  def reconstructWeights(weights: Matrix, xss: Seq[Seq[Double]]): Matrix =
    weights.fromSeqOfSeq(xss)

  def reconstructBiases(biases: Vector, xs: Seq[Double]): Vector =
    biases.fromSeq(xs)

  // TODO: dunno if we need this ... ?!? ...
  def reconstructLayer(layer: Layer, weights: Seq[Seq[Double]], biases: Seq[Double]) =
  layer.set(reconstructWeights(layer.weights, weights), reconstructBiases(layer.bias, biases))

  */

}