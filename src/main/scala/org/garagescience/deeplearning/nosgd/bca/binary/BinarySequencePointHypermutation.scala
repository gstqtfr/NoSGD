package org.garagescience.deeplearning.nosgd.bca.binary

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.ClassTag
import org.garagescience.deeplearning.nosgd.linalg.BinaryNumber
import scala.util.Random

// so we assume that all the conversion to a sequence of BinaryNumber has been
// handled in the class using this one.

// *all* we have to do is clone & mutate

// FIXME: we leave assessing the fitness to the calling class ...
// FIXME: NOT a sequence, we want the BinaryNumber mutator

// TODO: do we want to extend any traits here?
class BinarySequencePointHypermutation(bn: BinaryNumber,
                                       // TODO: what about sign bits?
                                       val mantissa: Int = 52,
                                       val poolSize: Int = 20,
                                       val maxHotSpots: Int = 10) {

  // TODO: this class needs to build the clonal pool itself
  // FIXME: DONE!!!
  var clones: IndexedSeq[BinaryNumber] = {
    for {i <- 0 until poolSize} yield bn
  }

  private val rand = Random

  // TODO: we also need to make with the mutation
  // TODO: multiple point mutation is the ticket here ...

  // TODO: could make this a one-liner ... (yikes!)
  private def getHotspots: Seq[Int] = {
    // randomly select the number of hot spots we'll
    // generate for this clone
    val hs: Int = rand.nextInt(maxHotSpots) + 1
    val hotSpots: Seq[Int] = (0 until hs).map { idx => rand.nextInt(mantissa) }
    hotSpots
  }


  // TODO: one thing i could do here is concentrate on a DoubleMatrix
  // TODO: but i'm reluctant to abandon the generic approach ...


  // TODO: wrong level here, i think; we need somhype on the BinaryNumber
  // TODO: this should work on the clones ...
  def germinate = clones.map(hypermutate)




  // we pass germinate a clone, & it performs hypermutation on it
  def hypermutate(bn: BinaryNumber) = {
    val _hotspots = getHotspots

  }


}
