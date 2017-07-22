package org.garagescience.deeplearning.nosgd.linalg

// this is how we've been performing double-to-bits conversions:

/*

// TODO: to replace some of these:

  def toBinaryString(d: Double): String =
    java.lang.Long.toBinaryString(java.lang.Double.doubleToRawLongBits(d))

  def fromBinaryString(s: String): Double =
    java.lang.Double.longBitsToDouble(new BigInteger(s, 2).longValue())

 */


// FIXME: & ANOTHER!!!

// java.lang.Long.toBinaryString(java.lang.Double.doubleToRawLongBits(d)).toList

// FIXME: EVEN BETTER!!!

// java.lang.Long.toBinaryString(java.lang.Double.doubleToRawLongBits(d)).toList.map { c => val n: BinaryNumber = c; n }

// FIXME: okay, that's cool. we *could* try pimping List, Seq, Array, Vector with these, have a toBinaryNumber
// FIXME: that might be a REALLY nice way of sorthing this out ...

// TODO: let's have a typeclass for the suckers ...

import java.math.BigInteger

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.language.higherKinds
import scala.collection.mutable.ListBuffer

// more usage:

/*
Array(1,0,0,1,1).map { elem =>
    val b: BinaryNumber = elem
    b
}
// Array[BinaryNumber] = Array(One$@710afd47, Zero$@64420e34, Zero$@64420e34, One$@710afd47, One$@710afd47)
 */

// from an array of chars, seq of longs

/*

Array('1','0','0','1','1').map { elem => val b: BinaryNumber = elem; b }
Seq(1L, 0L, 0L, 1L, 1L).map { elem => val b: BinaryNumber = elem; b }
 */

sealed trait BinaryNumber

final object Zero extends BinaryNumber

final object One extends BinaryNumber

// companion to build binary numbers
object BinaryNumber {

  // here's the question: do we wrap in an Option? or do
  // we throw an exception (yuk!)?
  // N.B. the default case is *very* cheeky
  implicit def int2Binary(n: Int): BinaryNumber = n match {
    case 0 => Zero
    case 1 => One
    case _ => Zero
  }

  implicit def long2Binary(l: Long): BinaryNumber = l match {
    case 0 => Zero
    case 1 => One
    case _ => Zero
  }

  implicit def char2Binary(c: Char): BinaryNumber = c match {

    case '0' => Zero
    case '1' => One
    case _ => Zero
  }

  // TODO: string, double implicits ...
}


// FIXME: the problem below is that we have to construct the list (or whatever)
// FIXME: on the R.H.S., can't go from a pre-existing list ...
// usage: val x: List[BinaryNumber] = List(1, 0, 1)
// => x: List[BinaryNumber] = List(One$@3c3a0032, Zero$@54e02f6a, Zero$@54e02f6a)
// val x: scala.collection.immutable.Vector[BinaryNumber] = scala.collection.immutable.Vector(0,1,1,1,0,0,1,0)
// val x: Seq[BinaryNumber] = Seq(0,1,1,1,0,0,1,0)
// works a treat - nifty ...

/*
object BinarySequence {

  import scala.collection.generic.CanBuildFrom

  implicit def toBinaryNumber[A: ClassTag, C[A] <: Traversable[A]](as: C[A])
                                                                  (implicit cbf: CanBuildFrom[C[A],
                                                                    A,
                                                                    C[A]]): C[A] = {

    as.toArray.to[C]
  }
}
*/


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

  private val data = checkAndModifyLength(_data).reverse


  // this deals with the problem that BigInteger.toString will return a +ve
  // number with the leading zeroes stripped; which is useless for our purposes
  private def checkAndModifyLength(l: ListBuffer[BinaryNumber]): ListBuffer[BinaryNumber] = l.length match {
    case `fixedLength` => _data
    case `positiveLength` =>
      _data.prepend('0','0')
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

}


// TODO: implicit classes? stick in an object?
// TODO: test *THE* *FUCK* out of this!!!

// TODO: yeah, doesn't work very well ... ah, well ...
// FIXME: when i have everything else behaving itself, FIX THIS!!!


// val tmp=java.lang.Long.toBinaryString(java.lang.Double.doubleToRawLongBits(d))
// val lbn1: List[BinaryNumber] = new BinaryVectorFromChar(tmp).data

object FixedBitVector {

  // usage: val bv = new BinaryVector(List(0,0,1,0,1,1,1,0))
  implicit class BinaryVectorFromInt(val _data: List[Int]) {

    def data: ListBuffer[BinaryNumber] = _data.map { elem =>
      val b: BinaryNumber = elem
      b
    }.to[ListBuffer]
  }

  // usage:
  // val d = - scala.util.Random.nextDouble
  // val xc = java.lang.Long.toBinaryString(java.lang.Double.doubleToRawLongBits(d)).toList
  // val bvfc = new BinaryVectorFromChar(xc)
  // val xs = bvfc.data

  implicit class BinaryVectorFromChar(val _data: List[Char]) {

    def data: ListBuffer[BinaryNumber] = _data.map { elem =>
      val b: BinaryNumber = elem
      b
    }.to[ListBuffer]
  }


  implicit class CharSeqFromBinaryVector(xs: List[BinaryNumber]) {

    def data: List[Char] = xs.map { elem: BinaryNumber =>
      elem match {
        case Zero => '0'
        case One => '1'
        case _ => '0'
      }
    }
  }

  implicit class IntSeqFromBinaryVector(xs: List[BinaryNumber]) {

    def data: List[Int] = xs.map { elem: BinaryNumber =>
      elem match {
        case Zero => 0
        case One => 1
        case _ => 0
      }
    }

  }

}

/*

// TODO: bollox. can;t get the fucker working ...

val xs: Seq[BinaryNumber] = Seq(0,1,1,1,0,0,1,0)

import scala.collection.generic.CanBuildFrom

val seqToBinaryNumberSeqBuilder = new CanBuildFrom[Seq[Int], BinaryNumber, Seq[BinaryNumber]] {
    def apply(from: Seq[Int]) = this.apply(); def apply() = Seq.newBuilder
}

val thingfish = xs.map(e=>e)(seqToBinaryNumberSeqBuilder)

//val bitsAgain = aintBits.map( _.toInt )( setToBitSetBuilder )

 */

