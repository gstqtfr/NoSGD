package org.garagescience.deeplearning.nosgd.linalg

import java.math.BigInteger
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.language.higherKinds
import scala.collection.mutable.ListBuffer


trait _FixedBitVector {

  // this fixed exponent keeps everything in a relatively small range
  // [-0.5, 0.5], which'll do for this experiment; it can get
  // arbitrarily close to [-1.0, 1.0]
  //val fixedExponent = Array(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
  val fixedExponent = Array(Zero, One, One, One, One, One, One, One, One, One, Zero)
  // IEEE 754 double-precision binary representation
  final val positiveLength = 62
  final val fixedLength = 64
  // the bits we can actually update, the mantissa & the sign bit
  final val length = 52

  def mantissa: Int = length
}

// TODO: we need to deal with the length of the buffer - why does
// TODO: the buffer.length == 62 when the double is +ve?

// TODO: prepend fixedExponent!

class FixedBitVector(_data: ListBuffer[BinaryNumber])
  extends _FixedBitVector {

  // TODO: we need modify the exponent here
  val data: ListBuffer[BinaryNumber] = checkAndModifyLength(_data).reverse

  // this deals with the problem that BigInteger.toString will return a +ve
  // number with the leading zeroes stripped; which is useless for our purposes
  private def checkAndModifyLength(l: ListBuffer[BinaryNumber]): ListBuffer[BinaryNumber] = l.length match {
    case `fixedLength` => _data
    case `positiveLength` =>
      _data.prepend('0', '0')
      _data
  }


  // we reverse so that we have a more natural ordering for
  // apply & update; we reverse back to show to the outside world!
  private def checkRangeConvert(index: Int) = {
    if ((index < 0) || (index >= length)) {
      throw new IndexOutOfBoundsException(s"index $index out of bounds")
    }
    index
  }

  // a specific method to flip between -ve & +ve?
  def unary_!() = data(63) = data(63) match {
    case One => Zero
    case Zero => One
  }

  /** The element at given index.
    *
    * Indices start at `0`; `xs.apply(0)` is the first element of array `xs`.
    * Note the indexing syntax `xs(i)` is a shorthand for `xs.apply(i)`.
    *
    * @param    index the index
    * @return the element at the given index
    * @throws       ArrayIndexOutOfBoundsException if `index < 0` or `length <= index`
    */

  def apply(index: Int): BinaryNumber = {
    val idx = checkRangeConvert(index)
    data(index)
  }

  /** Update the element at given index.
    *
    * Indices start at `0`; `xs.update(i, x)` replaces the i^th^ element in the array.
    * Note the syntax `xs(i) = x` is a shorthand for `xs.update(i, x)`.
    *
    * @param    index the index
    * @param    x     the value to be written at index `i`
    * @throws           ArrayIndexOutOfBoundsException if `index < 0` or `index >= length`
    */
  def update(index: Int, x: BinaryNumber): Unit = {
    val idx = checkRangeConvert(index)
    data(index) = x
  }


  override def toString(): String = data.map {
    case Zero => '0'
    case One => '1'
  }.reverse.mkString

  // TODO: an implicit here would be nice ...
  def toDouble(): Double = java.lang.Double.longBitsToDouble(new BigInteger(toString, 2).longValue())

  // provide a new copy of data for copying/cloning this object
  def copy: FixedBitVector = {

    val lb = new ListBuffer[BinaryNumber]()

    (0 until fixedLength).map { i =>
      // do we also need to copy BinaryNumber?
      lb += data(i)
    }

    new FixedBitVector(lb)
  }

}


// TODO: implicit classes? stick in an object?
// FIXME: in the meantime, let's use explicits ...

object FixedBitVector {

  def fromIntList(_data: List[Int]): ListBuffer[BinaryNumber] = _data.map { elem =>
    val b: BinaryNumber = elem
    b
  }.to[ListBuffer]


  def fromCharList(_data: List[Char]): ListBuffer[BinaryNumber] = _data.map { elem =>
    val b: BinaryNumber = elem
    b
  }.to[ListBuffer]


  def toCharList(xs: List[BinaryNumber]): List[Char] = xs.map { elem: BinaryNumber =>
    elem match {
      case Zero => '0'
      case One => '1'
      case _ => '0'
    }
  }


  def toIntList(xs: List[BinaryNumber]): List[Int] = xs.map { elem: BinaryNumber =>
    elem match {
      case Zero => 0
      case One => 1
      case _ => 0
    }
  }

}