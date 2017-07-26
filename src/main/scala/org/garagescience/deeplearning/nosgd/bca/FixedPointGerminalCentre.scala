package org.garagescience.deeplearning.nosgd.bca


class FixedPointGerminalCentre(override val d: Double,
                               override val poolSize: Int = 10)
  extends GerminalCentre(d, poolSize) with IntegralGerminalCentre[Array, Double, Double] {

  import Double2BitStringConvert._

  // this hypermutates our clonal pool & returns it
  /**
    * germinate hypermutates the clonal pool
    *
    * @return mutated clones of the original clonal pool
    */
  override def germinate: Array[Double] = clones.
    map { clone => somaticHypermutation(new StringBuffer(toBinaryString(clone))) }

  // FIXME: we need to start mutating at mantissa
  // FIXME: at the zero (so need to reverse)

  override def somaticHypermutation(_sb: StringBuffer): Double = {

    val length = scala.util.Random.nextInt(_sb.length)

    def _somaticHypermutation(sb: StringBuffer): Double = {

      for (i <- 0 until length)
        sb.setCharAt(index(i, sb.length), flipbit)

      fromBinaryString(sb.reverse.toString)
    }

    val __sb = _sb.reverse
    var _d: Double = 0

    do {
      _d = _somaticHypermutation(__sb)
    } while (java.lang.Double.isNaN(_d) || java.lang.Double.isInfinite(_d))

    _d
  }

}