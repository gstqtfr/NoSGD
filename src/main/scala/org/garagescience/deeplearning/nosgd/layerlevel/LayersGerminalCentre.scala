package org.garagescience.deeplearning.nosgd.layerlevel

import org.garagescience.deeplearning.nosgd.mlp.{Layer, LayerList, PartialLayer}
import org.jblas.DoubleMatrix

import scala.collection.immutable.{Seq => TSeq}
import scala.language.implicitConversions


// TODO: right. fuck it.

// TODO: WHERE DOES THE MEMORY CELL ENTER INTO THE ALGO?!?
// TODO: DO WE ADD THE HYPERMUTATED MATRIX?! OR REPLACE?!?!
// TODO: sort it out ...

// so, we assume that the list of layers has been initialised
// & we just have to clone & hypermutate it ...

// obviously, this implies that the network is at a higher level ...

// the purpose of the class is to
// a) take a list of layers & clone it
// b) hypermutate THE HELL out of the clone
// c) return it for evaluation

class LayersGerminalCentre(layerList: List[Layer],
                           val alpha: Double,
                           val eta: Double,
                           verbose: Boolean = false) {

  // we create our clones - copies of the layer list param.
  // these will be subjected to hypermutation

  /*
  def copyLayers: List[Layer] = {
    for {idx <- 0 until layerList.length}
      yield layerList(idx).copy()
  }.toList
  */

  /*
    // FIXME: what this needs to do is hypermutate the matices
    // FIXME: then return the clone of the layers
    // FIXME: this then gets
    // FIXME:     a) loaded up into the network
    // FIXME:     b) evaluated
    // FIXME: at a higher level ...
   */


  private def modifyWeight(m1: DoubleMatrix,
                           m2: DoubleMatrix): DoubleMatrix = m1.addi(m2.mul(alpha))


  // this simply hypermutates each matrix in the layer
  // FIXME: the question is: where do we evaluate it?
  private def germinateLayer(layer: Layer): Layer = {
    val clonedMatrix: DoubleMatrix = new MatrixGerminalCentre(layer.weights).germinate
    val weights: DoubleMatrix = modifyWeight(layer.weights, clonedMatrix)
    new Layer(weights, layer.activation)
  }

  def germinate: List[Layer] = layerList.map { layer => germinateLayer(layer) }




}