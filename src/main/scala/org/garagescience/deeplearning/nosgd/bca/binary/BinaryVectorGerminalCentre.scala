package org.garagescience.deeplearning.nosgd.bca.binary

import java.math.BigInteger

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.collection.mutable.ListBuffer
import org.garagescience.deeplearning.nosgd.linalg.{BinaryNumber, FixedBitVector, One, Zero}

import scala.collection.immutable.{IndexedSeq, Seq => TSeq}

// TODO: get it stamped out in plain, non-generic code 1st
// TODO: tidy up (asvtract to trait, higher kinds) later

// FIXME: so, i think the fitness function needs to be at the


class BinaryVectorGerminalCentre(xs: TSeq[Double]) {

  import FixedBitVector._

  private final def popSize = xs.length

  private def conv2CharList(d: Double): List[Char] =
    java.lang.Long.toBinaryString(java.lang.Double.doubleToRawLongBits(d)).toList

  private def convString2Double(s: String): Double =
    java.lang.Double.longBitsToDouble(new BigInteger(s, 2).longValue())

  private def createHypermutator(d: Double): BinarySequencePointHypermutation = {
    val lc: List[Char] = conv2CharList(d)
    val lbn: ListBuffer[BinaryNumber] = fromCharList(lc)
    new BinarySequencePointHypermutation(new FixedBitVector(lbn))
  }

  // create our population of binary number germinal centres
  private val gcs: TSeq[BinarySequencePointHypermutation] = xs.map { d => createHypermutator(d) }

  // iterate goes through the germinal centre & hypermutates the clones
  // the clones are returned & the Double representation is made available
  // to the calling object for evaluation ...

  def iterate: TSeq[TSeq[Double]] = {

    def _iterate: TSeq[TSeq[String]] = {
      val hyperMutatedClones: TSeq[TSeq[FixedBitVector]] = gcs.
        map(gc => gc.germinate)

      hyperMutatedClones.
        map(xsfb => xsfb.
          map(fbv => toCharList(fbv.data.toList).mkString))
    }

    _iterate.map(xs => xs.map(s => convString2Double(s)))

  }




}