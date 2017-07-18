import org.jblas.DoubleMatrix
import scala.language.implicitConversions
import org.garagescience.deeplearning.nosgd.linalg._
import org.garagescience.deeplearning.nosgd.linalg.Vector
import org.garagescience.deeplearning.nosgd.linalg.Matrix
import org.garagescience.deeplearning.nosgd.linalg.MatrixBuilder
import org.garagescience.deeplearning.nosgd.linalg.Vector.At

val array = Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0),Array(7.0, 8.0, 9.0))

// create a new jblas double matrix from the array
val jbm1 = new DoubleMatrix(array)

// we can build our linalg vector from Vectors
val v1 = Vector.at(0)(1.0,2.0,3.0)
val v2 = Vector.at(0)(4.0,5.0,6.0)

// now i need to create a Matrix from our linear algebra library
val builder = MatrixBuilder[Double]() at 0
builder += v1
builder += v2
val a = builder.result()


// TODO: right. this is a good place to start:
// TODO: stolen from: (good code here generally) ...
// https://github.com/axlelang/axle/blob/master/axle-jblas/
// ... src/main/scala/axle/jblas/package.scala


// TODO: this is getting somewhere:
jbm1.toArray.toList
// => List[Double] = List(1.0, 4.0, 7.0, 2.0, 5.0, 8.0, 3.0, 6.0, 9.0)
// FIXME: note that this list is knackered ... (i.e. column order) ...

def row(m: DoubleMatrix)(i: Int): DoubleMatrix =
  m.getRow(i)

row(jbm1)(0)
// => org.jblas.DoubleMatrix = [1.0, 2.0, 3.0]

// TODO: okay, put the last two together ...
row(jbm1)(0).toArray.toList
// => List[Double] = List(1.0, 2.0, 3.0)

// or:
row(jbm1)(0).toArray
// => Array[Double] = Array(1.0, 2.0, 3.0)

// FIXME: okay, lovely stuff. one fewer conversion. so, we can get
// FIXME: the data by each row; so we should be able to create a
// FIXME: Vector for each row, then create a Matrix[Double] from that!!!

// so let's have a crack at this:

def rowAsArray(m: DoubleMatrix)(i: Int): Array[Double] =
  m.getRow(i).toArray

rowAsArray(jbm1)(0)
// => Array[Double] = Array(1.0, 2.0, 3.0)

def doubleMatrix2LinalgMatrix(m: DoubleMatrix): Matrix[Double] = {

  val builder = MatrixBuilder[Double]() at 0
  val data = (0 until m.getRows) map { i =>
    rowAsArray(m)(i)
  }
  val vrep = (0 until m.getRows) map {i =>
    builder += Vector.at(0)(data(i): _*)
  }
  builder.result()
}

doubleMatrix2LinalgMatrix(jbm1)
// => org.garagescience.deeplearning.nosgd.linalg.Matrix[Double]
//     = ((1.0,2.0,3.0)@0,(4.0,5.0,6.0)@0,(7.0,8.0,9.0)@0)@0,0



// FIXME: christ ... okay, let's try again ...
// FIXME: okay, that's fine ...

object LinalgBLASImplicits {

    implicit def doubleBLASMatrix2LinalgMatrix(m: DoubleMatrix): Matrix[Double] = {

      def rowAsArray(m: DoubleMatrix)(i: Int): Array[Double] =
        m.getRow(i).toArray

      val builder = MatrixBuilder[Double]() at 0
      val data = (0 until m.getRows) map { i =>
        rowAsArray(m)(i)
      }
      val vrep = (0 until m.getRows) map {i =>
        builder += Vector.at(0)(data(i): _*)
      }
      builder.result()
    }

  implicit def linalgMatrix2DoubleBLASMatrix(m: Matrix[Double]): DoubleMatrix = {
    val jblas = DoubleMatrix.zeros(m.height, m.width)
    (0 until m.height) map {r =>
      (0 until m.width) foreach {c =>
        jblas.put(r, c, m(r,c))
      }

    }
    jblas
  }
}



val lm1: Matrix[Double] = jbm1
// => lm1: org.garagescience.deeplearning.nosgd.linalg.Matrix[Double]
//        = ((1.0,2.0,3.0)@0,(4.0,5.0,6.0)@0,(7.0,8.0,9.0)@0)@0,0

val tmp: DoubleMatrix = lm1
// => org.jblas.DoubleMatrix = [1.0, 2.0, 3.0; 4.0, 5.0, 6.0; 7.0, 8.0, 9.0]

// TODO: JOB DONE!!!
