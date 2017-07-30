package org.garagescience.deeplearning.nosgd.layerlevel

import org.garagescience.deeplearning.nosgd.mlp.{Layer, LayerList, PartialLayer}
import org.jblas.DoubleMatrix

import scala.collection.immutable.{Seq=>TSeq}
import scala.language.implicitConversions

// so, we assume that the list of layers has been initialised
// & we just have to clone & hypermutate it ...

// obviously, this implies that the network is at a higher level ...

class LayerGerminalCentre(layerList: LayerList,
                          val alpha: Double,
                          val eta: Double,
                          val poolSize: Int = 10,
                          verbose: Boolean = false) {

  // we create our clones - copies of the layer list param.
  // these will be subjected to hypermutation
  var layerClones: TSeq[List[Layer]] = (0 until poolSize).map { idx =>
    layerList.layers.map { _.copy() }
  }

  // this simply hypermutates each matrix in the layer
  def germinate(layers: List[Layer]) = {
    layers.map { (layer: Layer) =>
      layer.weights
    }
  }







}