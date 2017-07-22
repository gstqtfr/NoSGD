import org.garagescience.deeplearning.nosgd.linalg._

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.language.higherKinds

import BinaryNumber._
import FixedBitVector._

val d = scala.util.Random.nextGaussian
val xc = java.lang.Long.toBinaryString(java.lang.Double.doubleToRawLongBits(d)).toList
val fbv1 = new FixedBitVector(new BinaryVectorFromChar(xc).data)
fbv1.toString
fbv1.toDouble

// N.B. this'll only do something if fbv1(0) == One!
fbv1(0) = Zero
fbv1.toDouble

fbv1(0) = One
fbv1.toDouble

// works perfick!

// test the unary ! operator
!fbv1
fbv1.toString()
fbv1.toDouble

!fbv1
fbv1.toString()
fbv1.toDouble