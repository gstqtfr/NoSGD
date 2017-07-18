package org.garagescience.deeplearning.nosgd.bca

// val MAX_ABSOLUTE_WEIGHT_VALUE: Double = 1.0

class ConstrainedGerminalCentre(override val d: Double,
                                private val MAX_ABSOLUTE_WEIGHT_VALUE: Double = 1.0,
                                override val poolSize: Int = 20)
  extends GerminalCentre(d, poolSize) with IntegralGerminalCentre[Double, Double] {

  import Double2BitStringConvert._



  // this hypermutates our clonal pool & returns it
  /**
    * germinate hypermutates the clonal pool
    *
    * @return mutated clones of the original clonal pool
    */
  override def germinate: Array[Double] = clones.
    map{ clone =>
      var candidate = 0.0
      do {
        candidate = somaticHypermutation(new StringBuffer(toBinaryString(clone)))
      } while (!isInRange(candidate))
      candidate
    }

  def isInRange(d: Double) = -1.0 <= d && d <= 1.0


}
