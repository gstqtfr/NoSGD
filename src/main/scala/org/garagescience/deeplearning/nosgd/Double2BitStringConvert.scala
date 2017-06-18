package org.garagescience.deeplearning.nosgd

import java.math.BigInteger

object Double2BitStringConvert {

  // TODO: add one for Seq's?

  def toBinaryString(d: Double): String =
    java.lang.Long.toBinaryString(java.lang.Double.doubleToRawLongBits(d))

  def fromBinaryString(s: String): Double =
    java.lang.Double.longBitsToDouble(new BigInteger(s, 2).longValue())

  // test this. random numbers, assert that they're the same.

  def testConversion(sz: Int=100): Array[Boolean] =
    (1 to sz).map { i =>
      val d1 = scala.util.Random.nextGaussian()
      val d2 = fromBinaryString(toBinaryString(d1))
      d1 == d2
    }.toArray

}

