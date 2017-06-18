import org.garagescience.deeplearning.nosgd._
import org.apache.spark.ml.linalg.{Matrices, Matrix}
import org.apache.spark.ml.linalg.{Vectors, Vector}

import Matrix2BinarySeq._
val randomSeq = (0 until 9).map { i => scala.util.Random.nextGaussian}
val m = Matrices.dense(3, 3, randomSeq.toArray)

val v = Vectors.dense(randomSeq.toArray)