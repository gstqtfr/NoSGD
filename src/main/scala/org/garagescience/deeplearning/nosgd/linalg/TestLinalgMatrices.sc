/**
  * Created by jkelsey on 02/07/2017.
  */


import org.garagescience.deeplearning.nosgd.linalg._
import  _MatrixImplicits._

def randomMatrix[Double](rows: Int, cols: Int, sz: Int): _DenseMatrix[Double] = {
  val tmpArray = (for {i <- 0 until sz}
    yield scala.util.Random.nextGaussian).toArray.asInstanceOf[Array[Double]]
  new _DenseMatrix[Double](rows, cols, tmpArray, false)
}

import breeze.linalg._

def toRandomBreeze(rows: Int, cols: Int, sz: Int): DenseMatrix[Double] = {
  val tmpArray: Array[Double] = (for {i <- 0 until sz} yield scala.util.Random.nextGaussian).toArray
  new DenseMatrix[Double](rows, cols, tmpArray)
}

val _dm1: _DenseMatrix[Double] = randomMatrix(3, 3, 3*3)

// test the breeze implicit

// ... breeze implicits ...
val bm1: Matrix[Double] = _dm1
// an explicit call of the implicit; we choose the type
val bm2: Matrix[Double] = _dm1.asBreeze


// create a random Breeze matrix
val bm3 = toRandomBreeze(3, 3, 3*3)

// test the explicit:
_Matrix.fromBreeze(bm3)

// works fine!

// test the implicit, this is from the Breeze matrix... perfect ...
val _dm2: _DenseMatrix[Double] = bm3





