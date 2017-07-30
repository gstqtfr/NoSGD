package org.garagescience.deeplearning.nosgd.bca

import scala.IndexedSeq
import scala.collection.immutable.IndexedSeq
import scala.language.implicitConversions
import scala.reflect.ClassTag
import org.jblas.DoubleMatrix


class MemoryCell(max: Int, alpha: Double, m: DoubleMatrix) {

  import Shift._
  import Decay._

  var matrixArray: Array[DoubleMatrix] = (0 until max).map { idx => m }.toArray

  def pipeline(m: DoubleMatrix): Array[DoubleMatrix] = {
    matrixArray = decay(alpha, shift(m, matrixArray))
    matrixArray
  }


}



object Shift {

  // FIXME: blah blah generic collection type ...
  def shift[T: ClassTag](elem: T, xs: Array[T]): Array[T] = {
    val xs1 = elem +: xs
    xs1.dropRight(1)
  }
}

// TODO: er, type params? or type class?
object Decay {

  def _decay(coeff: Double, i: Double, d: Double) = Math.pow(coeff, i) * d

  // straighforward array implementation
  def decay(coeff: Double, array: Array[Double]): Array[Double] = {

    (0 until array.length).map { idx =>
      _decay(coeff, idx, array(idx))
    }.toArray
  }

  def _decay(coeff: Double, i: Double, d: DoubleMatrix) = d.mmul(Math.pow(coeff, i))

  // & another! this one works on BLAS matrices ...
  def decay(coeff: Double, mArray: Array[DoubleMatrix]): Array[DoubleMatrix] = {

    (0 until mArray.length).map { idx =>
      _decay(coeff, idx, mArray(idx))
    }.toArray
  }

}





// TODO: method names?

import scala.collection._
import mutable.ListBuffer

// this works okay, prob. worth keeping around ...
class FixedList[A](max: Int) extends Traversable[A] {

  val list: ListBuffer[A] = ListBuffer()

  def append(elem: A)  {
    if (list.size == max) {
      list.trimEnd(1)
    }
    list.prepend(elem)
  }

  def foreach[U](f: A => U) = list.foreach(f)
}



/*

class MemoryCell[A](max: Int) {

  var fixedList = new FixedList[A](max)

  def decay(i: Int, d: Double, e: Double) = Math.pow(e, i) * d

  // append has to apply f to the list's elements
  // while appending the new element

  def append[U](elem: A)(u: U)(f: (U,A) => A) = {
    fixedList.append(f(u, elem))
  }
}

*/

// def decay(i: Int, d: Double, e: Double) = Math.pow(e, i) * d






// TODO: i don't seem to be in the fucking mood today ...
// TODO: okay, here's how to do it using an array

/*

// create it
val a = Array(2.0, 1.0, 0.0)
// add a new elem
val b = 3.0 +: a
// get rid of the tail
val c = b.drop(1)
// => Array[Double] = Array(3.0, 2.0, 1.0)

// right. so what we need to do next is to do a map on the elements
// mult. by alpha
// & return the adapted list




 */




// TODO: matrix usage:

/*

import org.garagescience.deeplearning.nosgd.bca._
import Shift._
import Decay._
import org.jblas.DoubleMatrix._

var array=(0 until 3).map { idx => randn(2,2) }.toArray

array=decay(0.01, shift(randn(2,2), array))


 */






/*

import org.garagescience.deeplearning.nosgd.bca._
import Shift._
import Decay._

var array = Array(3.0, 2.0, 1.0)

decay(0.03, array)
// => Array[Double] = Array(3.0, 0.06, 9.0E-4)

shift(4.0, array)
// => Array[Double] = Array(4.0, 3.0, 2.0)

// all together now (all together now!)

decay(0.01, shift(4.0, array))
// => Array[Double] = Array(4.0, 0.03, 2.0E-4)




*/








/*

import org.garagescience.deeplearning.nosgd.bca._
var array = Array(3.0, 2.0, 1.0)
val wanker = new WANKER(0.03, array)

wanker.decay
// => res0: Array[Double] = Array(3.0, 0.06, 9.0E-4)

// TODO: have a rest, then tie these guys together. works just fine.

(0 until 3).map { i =>
     val d = 1.0
     val e = 0.03
     println(s"d == $d, e == $e")
     val out = decay(i, d, e)
     println(s"result: $out")
}
 */








// TODO: usage:
/*
val fl = new FixedList[Int](3)
//fl: FixedList[Int] = ()

fl.append(1)

fl.append(2)

fl.append(3)

fl
//res28: FixedList[Int] = (3, 2, 1)

fl.append(4)

fl
//res30: FixedList[Int] = (4, 3, 2)

// TODO: this is PERFECT!!!!!
 */


