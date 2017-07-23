import org.garagescience.deeplearning.nosgd.bca.binary._

val target=(0 until 3).map { i => scala.util.Random.nextGaussian }
val bvgc = new BinaryVectorGerminalCentre(target)
