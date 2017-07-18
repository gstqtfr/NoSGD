package org.garagescience.deeplearning.nosgd.XOR

import scala.collection.mutable.Buffer
import breeze.linalg.{DenseMatrix, DenseVector, max}
import math.abs

class SomHypeNeuralNetwork(_neuronCounts: Seq[Int],
                           _activationFunction: ActivationFunction,
                           val popSize: Int = 20,
                           override val useBias: Boolean = true)
  extends _NeuralNetwork(_neuronCounts = _neuronCounts, useBias = useBias) {

  // TODO: do we need this? ...
  def activationFunction = _activationFunction

  // TODO: move to the parent class?
  def getMaxDelta(): Double = delta.tail.map(_.map(abs(_)).max).max

  // TODO: how shall i choose to do this?
  // TODO: we need eta, V, activationFunction.derivative, & h

  // TODO: maybe we should pull all this apart a bit more
  def trainImpl(input: Seq[Double], desiredResult: Seq[Double]) = {
    assert(input.length == V(0).length - 1)
    assert(desiredResult.length == V(M).length)

    classify(input) //sets V and h

    val eta: DenseVector[Double] = DenseVector(desiredResult: _*) - V(M)

    delta(M) := getError(eta, h(M), activationFunction.derivative)

    // this returns the error of the output layer ...
    delta(M)
  }





    // this be the error: N.B., M is layerCount - 1
    // delta(M) := h(M).map(activationFunction.derivative) *:* (eta - V(M))

    // we could also get the layer-by-layer backprop error, but let's go for
    // somatic hypermutation from here

    // so our targets are the weight matrix, which is def'd by w, where
    // w is a buffer of DenseMatrix[Double]. which we know how to Do
    // Stuff to ...


    // TODO: THIS!

    /*
     //adjusting the weights
    for(m <- Range(1, M).inclusive) {
      val weightAdjustments = (delta(m) * V(m-1).t :* gamma)
      w(m-1) := w(m-1) + weightAdjustments
    }
     */

    // so we need to send these to clonal expansion actors & mutate 'em
    // & return the one with the least error ...

    // do we have an error function?

    // yeah: we have delta, as worked out below:

    /*

    val eta: DenseVector[Double] = DenseVector(desiredResult : _*)

    //backpropagation - last layer
    // this be the error: N.B., M is layerCount - 1
    delta(M) := h(M).map(activationFunction.derivative) *:* (eta - V(M))

    // TODO: so we can pass in eta, classifyImpl gives us V(M)
    // TODO: & we can also pass in activationFunction.derivative & h(M)
    // TODO: this is quite a bit to pass in to a function, but ...
    // TODO: anyway, it'll be a 1st draft of the function ...

     */


    /* okay, here's the thing. we can adjust ALL the weight matrices, just
      given the delta(M) (which are the weights of the output layer)
     */







}