package org.garagescience.deeplearning.nosgd.bca.binary

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.ClassTag
import org.garagescience.deeplearning.nosgd.linalg.{BinaryNumber, FixedBitVector, One, Zero}

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

  // build the clonal pool
  var clones: IndexedSeq[FixedBitVector] = {
    for {i <- 0 until poolSize} yield fbv
  }

  private val rand = Random

  // TODO: could make this a one-liner ... (yikes!)

  private def getHotspots: Seq[Int] = {
    // randomly select the number of hot spots we'll
    // generate for this clone
    val hs: Int = rand.nextInt(maxHotSpots) + 1
    val hotSpots: Seq[Int] = (0 until hs).map { idx => rand.nextInt(fbv.mantissa) }
    hotSpots
  }

  // with P(0.5), flip the bit in the bit vector
  private def hotspot: BinaryNumber = if (rand.nextDouble <= 0.5) Zero else One

  // TODO: one thing i could do here is concentrate on a DoubleMatrix
  // TODO: but i'm reluctant to abandon the generic approach ...

  // update the clones by applying hypermutation on them,
  // and then update them ...
  def germinate: Seq[FixedBitVector] = {
    clones = clones.map(hypermutate)
    clones
  }

  // we could one-liner this, too ... (?!?)
  // we pass germinate a clone, & it performs hypermutation on it
  def hypermutate(clone: FixedBitVector) = {
    val _hotspots = getHotspots
    (0 until _hotspots.length).map { idx =>
      clone(idx) = hotspot
    }
    clone
  }
  
}
