package org.garagescience.deeplearning.nosgd.mlp

import org.jblas.DoubleMatrix
import org.jblas.MatrixFunctions._
import scala.language.higherKinds

// TODO: need to higher-kind this ...

trait DifferentiableFunction extends Serializable {
  def derivative(x: DoubleMatrix, y: DoubleMatrix): DoubleMatrix
}

