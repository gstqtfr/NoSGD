package org.garagescience.deeplearning.nosgd.bca.binary

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.ClassTag
import org.garagescience.deeplearning.nosgd.linalg.{BinaryNumber, FixedBitVector, One, Zero}
import scala.collection.immutable.{IndexedSeq, Seq=>TSeq}
import scala.util.Random

// so we assume that all the conversion to a sequence of BinaryNumber has been
// handled in the class using this one.

// *all* we have to do is clone & mutate

// FIXME: we leave assessing the fitness to the calling class ...
// FIXME: NOT a sequence, we want the BinaryNumber mutator

// TODO: do we want to extend any traits here?
class BinarySequencePointHypermutation(fbv: FixedBitVector,
                                       // TODO: what about sign bits?
                                       val poolSize: Int = 20,
                                       val maxHotSpots: Int = 10) {

  var clones: IndexedSeq[FixedBitVector] =
    // need a deep copy for this
    (0 until poolSize).map { idx => fbv.copy }


  private val rand = Random

  // could make this a one-liner ... (yikes!)

  private def getHotspots: TSeq[Int] = {
    // randomly select the number of hot spots we'll
    // generate for this clone
    val hs: Int = rand.nextInt(maxHotSpots) + 1
    val hotSpots: TSeq[Int] = (0 until hs).map { idx => rand.nextInt(fbv.mantissa) }
    hotSpots
  }

  // with P(0.5), flip the bit in the bit vector
  private def hotspot: BinaryNumber = if (rand.nextDouble <= 0.5) Zero else One

  // update the clones by applying hypermutation on them,
  // and then update them ...
  def germinate: TSeq[FixedBitVector] = {
    for {clone <- clones} yield hypermutate(clone)
    clones
  }

  // we could one-liner this, too ... (?!?)
  // we pass germinate a clone, & it performs hypermutation on it
  def hypermutate(clone: FixedBitVector): FixedBitVector = {
    val _hotspots = getHotspots
    (0 until _hotspots.length).map { idx =>
      clone(idx) = hotspot
    }
    clone
  }

}
