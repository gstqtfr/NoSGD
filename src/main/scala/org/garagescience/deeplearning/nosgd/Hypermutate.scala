package org.garagescience.deeplearning.nosgd

import scala.collection.immutable.{IndexedSeq, Seq}
import Double2BitStringConvert._


trait Hypermutate {

  import Double2BitStringConvert._

  protected val r = new scala.util.Random
  protected def index(idx: Int, length: Int): Int = idx % length
  protected def flipbit: Char = if (r.nextDouble >= 0.5) '1' else '0'
  protected def siteAndLength(L: Int): (Int, Int) = (r.nextInt(L), r.nextInt(L))

  protected def somaticHypermutation(_sb: StringBuffer): Double = {
    def _somaticHypermutation(sb: StringBuffer): Double = {

      val (length, hotspot) = siteAndLength(sb.length)

      for (i <- hotspot to length + hotspot)
        sb.setCharAt(index(i, sb.length), flipbit)

      fromBinaryString(sb.toString)
    }

    // todo: WTF? WHAT IS THIS? OMFG! a VAR! whatever shall we do?!?!?
    var _d: Double = 0

    do {
      _d = _somaticHypermutation(_sb)
    } while (java.lang.Double.isNaN(_d) || java.lang.Double.isInfinite(_d))

    _d
  }

  // TODO: should we get rid of this? or ove it to another trait?
  protected def clonalPool(sb: StringBuffer)(sz: Int = 20): Seq[Double] = {

    val clone: Seq[StringBuffer] =
      (for {i <- 1 to sz} yield new StringBuffer(sb.toString)) :+ sb
    clone.map(somaticHypermutation)
  }

}