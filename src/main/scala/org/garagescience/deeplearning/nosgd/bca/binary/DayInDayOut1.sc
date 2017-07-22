import org.garagescience.deeplearning.nosgd.linalg._

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.language.higherKinds

import BinaryNumber._
import FixedBitVector._

val d = scala.util.Random.nextGaussian
val xc = java.lang.Long.toBinaryString(java.lang.Double.doubleToRawLongBits(d)).toList
val fbv1 = new FixedBitVector(new BinaryVectorFromChar(xc).data)