import org.garagescience.deeplearning.nosgd.bca._

import scala.collection.immutable.IndexedSeq

val candidate: Array[Double] = {
  for (i <- 0 until 10) yield scala.util.Random.nextGaussian
}.toArray

def creatTarget(d: Double): Array[Double] = {
  for (i <- 0 until 10) yield d
}.toArray

val target = creatTarget(Math.sqrt(2))

val c = new ConstrainedSequenceHypermutation(candidate)

def getScore(xs1: Array[Double], xs2: Array[Double]) =
  xs1.zip(xs2).map { case (u,v) => u - v }

// TODO: begin iterating here

(0 until 5).foreach {iteration =>
  val clonalPool = c.clones

  val currentFitness: Array[Double] = (0 until clonalPool.length).map { i =>
    getScore(clonalPool(i), target).map(Math.abs).sum
  }.toArray

  val newClonalPool: Array[Array[Double]] = c.germinate

  val newFitness: Array[Double] = (0 until newClonalPool.length).map { i =>
    getScore(newClonalPool(i), target).map(Math.abs).sum
  }.toArray

  c.update(currentFitness, newFitness, newClonalPool)

  (0 until newFitness.length).map { idx =>
    println(s"$iteration ${currentFitness(idx)} ${newFitness(idx)}")
  }

}