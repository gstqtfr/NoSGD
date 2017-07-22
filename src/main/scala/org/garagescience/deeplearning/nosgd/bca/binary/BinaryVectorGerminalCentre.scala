package org.garagescience.deeplearning.nosgd.bca.binary

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.collection.mutable.ListBuffer
import org.garagescience.deeplearning.nosgd.linalg.{BinaryNumber, FixedBitVector, One, Zero}

import scala.collection.immutable.{IndexedSeq, Seq}

// TODO: get it stamped out in plain, non-generic code 1st
// TODO: tidy up (asvtract to trait, higher kinds) later


class BinaryVectorGerminalCentre(xs: Seq[Double]) {

  import FixedBitVector._

  private final def popSize = xs.length

  private def conv2CharList(d: Double): List[Char] =
    java.lang.Long.toBinaryString(java.lang.Double.doubleToRawLongBits(d)).toList


  private def createHypermutator(d: Double): BinarySequencePointHypermutation = {
    val lc: List[Char] = conv2CharList(d)
    val lbn: ListBuffer[BinaryNumber] = fromCharList(lc)
    new BinarySequencePointHypermutation(new FixedBitVector(lbn))
  }

  // create our population of binary number germinal centres
  private val gcs: Seq[BinarySequencePointHypermutation] = xs.map { d => createHypermutator(d) }







}