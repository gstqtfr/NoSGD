package org.garagescience.deeplearning.nosgd.mlp

import org.garagescience.deeplearning.nosgd.mlp.data.DataSet
import org.jblas.DoubleMatrix

import scala.collection.immutable.{Seq => TSeq}

class SomHypeNeuralNetwork(override val layers: List[Layer],
                           override val objective: ObjectiveFunction)
  extends NeuralNetwork with Serializable {

  def copy(layers: List[org.garagescience.deeplearning.nosgd.mlp.Layer]): SomHypeNeuralNetwork =
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

}

object SomHypeNeuralNetwork {

  import java.io._


  def apply(layers: List[Layer], objective: ObjectiveFunction): SomHypeNeuralNetwork = {
    new SomHypeNeuralNetwork(layers, objective)
  }

  // TODO: look at other params. choices - should we provide alpha, eta?!
  /*
  def apply(layers: List[Layer], objective: ObjectiveFunction, weightDecay: Double = 0.0): SomHypeNeuralNetwork = {
    new SomHypeNeuralNetwork(layers, objective, weightDecay)
  }
  */

  def printDimensions(n: String, m: DoubleMatrix) {
    println("Matrix '%s': %dx%d".format(n, m.rows, m.columns))
  }

  def save(network: SomHypeNeuralNetwork, file: String) {
    val os = new ObjectOutputStream(new FileOutputStream(file))
    try {
      os.writeObject(network)
    } finally {
      os.close()
    }
  }

  def load(file: String): SomHypeNeuralNetwork = {
    val is = new ObjectInputStream(new FileInputStream(file))
    try {
      is.readObject().asInstanceOf[SomHypeNeuralNetwork]
    } finally {
      is.close()
    }
  }

}