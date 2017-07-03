/**
  * Created by jkelsey on 02/07/2017.
  */


import org.garagescience.deeplearning.nosgd.linalg._
import  _MatrixImplicits._

def randomMatrix(rows: Int, cols: Int, sz: Int): _DenseMatrix = {
  val tmpArray: Array[Double] = (for {i <- 0 until sz} yield scala.util.Random.nextGaussian).toArray
  new _DenseMatrix(rows, cols, tmpArray, false)
}

import breeze.linalg._

def toRandomBreeze(rows: Int, cols: Int, sz: Int): DenseMatrix[Double] = {
  val tmpArray: Array[Double] = (for {i <- 0 until sz} yield scala.util.Random.nextGaussian).toArray
  new DenseMatrix[Double](rows, cols, tmpArray)
}

val _dm1 = randomMatrix(3, 3, 3*3)

// test the breeze implicit

// ... breeze implicits ...
val bm1: Matrix[Double] = _dm1
// an explicit call of the implicit; we choose the type
val bm2: Matrix[Double] = _dm1.asBreeze

// now we test the ml implicits
val ml1: Matrix[Double] = _dm1
// ... now explicitly calling the implicit (?!?) ...
// no type info needed, it's inferred as org.apache.spark.ml.linalg.DenseMatrix
val ml2  = _dm1.asML

// create a random Breeze matrix
val bm3 = toRandomBreeze(3, 3, 3*3)

// test the explicit:
_Matrix.fromBreeze(bm3)

// works fine!

// test the implicit, this is from the Breeze matrix... perfect ...
val _dm2: _DenseMatrix = bm3

// now we need to test the ML Matrix, here goes:
val _dm3: _DenseMatrix = ml2

// yep: all working perfectly ...