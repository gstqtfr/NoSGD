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

// ... actually, this thing should do a clone of the layers themselves
// & return that ...

class LayersGerminalCentre(layerList: List[Layer],
                           val alpha: Double,
                           val eta: Double,
                           verbose: Boolean = false) {

  // we create our clones - copies of the layer list param.
  // these will be subjected to hypermutation

  // TODO: this can be val'd
  var layerListClone: List[Layer] = { for {idx <- 0 until layerList.length}
    yield layerList(idx).copy()
  }.toList




  def germinateLayer = ???


  /*
    // FIXME: what this needs to do is hypermutate the matices
    // FIXME: then return the clone of the layers
    // FIXME: this then gets
    // FIXME:     a) loaded up into the network
    // FIXME:     b) evaluated
    // FIXME: at a higher level ...
   */






  // TODO: add THE MO!!! we need THE MO!!!!
  def modifyWeight(m1: DoubleMatrix, m2: DoubleMatrix): DoubleMatrix =
    m1.addi(m2.mul(alpha))


  // TODO: okay, what we need is ... a bloody break from all these
  // TODO: fucking interviews & phone calls. then i can relax &
  // TODO: focus on what needs doing.

  // this simply hypermutates each matrix in the layer
  // FIXME: the question is: where do we evaluate it?
  def germinate(_layers: List[Layer]): List[DoubleMatrix] = {
    _layers.map { (layer: Layer) =>
      // FIXME: *addition* of hypermutated weights? prob. ...
      // FIXME: the mo! do we have the mo? WE NEED THE MO!

      // hypermutate the matrix
      // FIXME: so, do we refactor this? object?
     val clonedMatrix = new MatrixGerminalCentre(layer.weights).germinate
      modifyWeight(layer.weights, clonedMatrix)
    }
  }







}