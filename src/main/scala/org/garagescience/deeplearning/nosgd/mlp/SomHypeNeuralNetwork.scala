package org.garagescience.deeplearning.nosgd.mlp

// TODO: let's copy AS MUCH AS WE CAN across & adapt it

import org.garagescience.deeplearning.nosgd.mlp.data.DataSet
import org.jblas.DoubleMatrix

import scala.collection.immutable.{Seq => TSeq}

class SomHypeNeuralNetwork(override val layers: List[Layer],
                           override val objective: ObjectiveFunction)
  extends NeuralNetwork with Serializable {

  def copy(layers: List[org.garagescience.deeplearning.nosgd.mlp.Layer]): NeuralNetwork =
    new SomHypeNeuralNetwork(layers, objective)

  def loss(data: org.garagescience.deeplearning.nosgd.mlp.data.DataSet): Double = {
    val outputs: List[LayerState] = forwardProp(data.inputs)
    // this doesn't apply the weight decay strategy
    objective(outputs.last.activationOutput, data.targets)
  }

  // TODO: so, what does this matrix represent? okay, it works like this:
  // TODO: x.sub(y).muli(1.0 / y.columns)
  // TODO: where x == outputs.last.activationOutput
  // TODO: &     y == data.targets
  // TODO: where, clearly, we get an average error term
  // TODO: (take the target away from the target, divide n (# of targets)


  def errorGradients(data: DataSet): DoubleMatrix = {

    val outputs: List[LayerState] = forwardProp(data.inputs)
    val errorDerivative: DoubleMatrix = objective.derivative(outputs.last.activationOutput, data.targets)

    errorDerivative
  }


  // FIXME: COPY!!!

/*
  def errorGradients(data: DataSet): Seq[DoubleMatrix] = {

    // TODO: look at the two lines of code below ... BINGO ...
    // TODO: we could just use these guys to produce the eval. of the weights
    // TODO: for the hypermutated clones ...

    // TODO: START HERE
    val outputs: List[LayerState] = forwardProp(data.inputs)
    val errorDerivative: DoubleMatrix = objective.derivative(outputs.last.activationOutput, data.targets)
    // TODO: STOP HERE

    val derivatives = (0 until layers.size).scanRight(errorDerivative) {
      case (i, priorDerivative) =>
        val priorDerivativeWeighted = if (i < layers.size - 1) {
          layers(i + 1).weights.mmul(priorDerivative)
        } else priorDerivative
        val derivative = layers(i).activation.derivative(outputs(i).compositionOutput.get, outputs(i).activationOutput)
        derivative.mul(priorDerivativeWeighted)
    }

    derivatives.zipWithIndex.map {
      case (derivative, i) =>
        val gradient = if (i > 0) {
          outputs(i - 1).activationOutput.mmul(derivative.transpose)
        } else {
          data.inputs.mmul(derivative.transpose)
        }


        gradient
    }
  }

  */

}