package org.garagescience.deeplearning.nosgd.mlp

import org.garagescience.deeplearning.nosgd.mlp.data.DataSet
import org.jblas.DoubleMatrix
import org.jblas.MatrixFunctions._

class BackPropNeuralNetwork(override val layers: List[Layer],
                            override val objective: ObjectiveFunction,
                            weightDecay: Double) extends NeuralNetwork with Serializable {

  @transient val decay = new WeightDecay(weightDecay)

  def copy(layers: List[Layer]): BackPropNeuralNetwork =
    new BackPropNeuralNetwork(layers, objective, weightDecay)


  // TODO: right, the stuff below is responsible for the main adaptation of the
  // TODO: of the network. it's here we get most of the error eval. & weight
  // TODO: modification. so once we crack this ...

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
        if (i < layers.size) {
          decay.derivative(layers(i), gradient)
        } else gradient
    }
  }




  def loss(data: DataSet): Double = {
    val outputs = forwardProp(data.inputs)
    decay(layers, objective(outputs.last.activationOutput, data.targets))
  }

}

object BackPropNeuralNetwork {
  import java.io._

  def apply(layers: List[Layer], objective: ObjectiveFunction, weightDecay: Double = 0.0): BackPropNeuralNetwork = {
    new BackPropNeuralNetwork(layers, objective, weightDecay)
  }

  def printDimensions(n: String, m: DoubleMatrix) {
    println("Matrix '%s': %dx%d".format(n, m.rows, m.columns))
  }

  def save(network: BackPropNeuralNetwork, file: String) {
    val os = new ObjectOutputStream(new FileOutputStream(file))
    try {
      os.writeObject(network)
    } finally {
      os.close()
    }
  }

  def load(file: String): BackPropNeuralNetwork = {
    val is = new ObjectInputStream(new FileInputStream(file))
    try {
      is.readObject().asInstanceOf[BackPropNeuralNetwork]
    } finally {
      is.close()
    }
  }
}