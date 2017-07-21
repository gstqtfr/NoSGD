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

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.language.higherKinds

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

object BinarySequence {

  import scala.collection.generic.CanBuildFrom

  implicit def toBinaryNumber[A: ClassTag, C[A] <: Traversable[A]](as: C[A])
                                                                  (implicit cbf: CanBuildFrom[C[A], A, C[A]]): C[A] = {

    as.toArray.to[C]
  }
}


trait FixedBitVector[C[A] <: Traversable[A]] {

  val data: C[BinaryNumber]

  // this fixed exponent keeps everything in a relatively small range
  // [-0.5, 0.5], which'll do for this experiment; it can get
  // arbitrarily close to [-1.0, 1.0]
  //val fixedExponent = Array(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
  val fixedExponent = Array(Zero, One, One, One, One, One, One, One, One, One, Zero)
  // IEEE 754 double-precision binary representation
  final val fixedLength = 64
  // the bits we can actually update, the mantissa & the sign bit
  final val length = 52

}

// TODO: implicit classes? stick in an object?

// usage: val bv = new BinaryVector(List(0,0,1,0,1,1,1,0))
class BinaryVectorFromInt(val _data: List[Int]) extends FixedBitVector[List] {

  override val data: List[BinaryNumber] = _data.map { elem =>
    val b: BinaryNumber = elem
    b
  }
}

// usage:
// val d = - scala.util.Random.nextDouble
// val xc = java.lang.Long.toBinaryString(java.lang.Double.doubleToRawLongBits(d)).toList
// val bvfc = new BinaryVectorFromChar(xc)

class BinaryVectorFromChar(val _data: List[Char]) extends FixedBitVector[List] {

  override val data: List[BinaryNumber] = _data.map { elem =>
    val b: BinaryNumber = elem
    b
  }
}


class CharSeqFromBinaryVector(xs: List[BinaryNumber]) {

  val data: List[Char] = xs.map { elem: BinaryNumber =>
    elem match {
      case Zero => '0'
      case One => '1'
      case _ => '0'
    }
  }
}

class IntSeqFromBinaryVector(xs: List[BinaryNumber]) {

  val data: List[Int] = xs.map { elem: BinaryNumber =>
    elem match {
      case Zero => 0
      case One => 1
      case _ => 0
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









