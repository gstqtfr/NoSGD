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

getScore(candidate, target)

// TODO: this needs to be calc'd every time we iterate over all the
// TODO: patterns

// var fitness = getScore(candidate, target)

// FIXME: this is f


// FIXME: this clones a single double many times over
// FIXME: we need it to clone the whole array ...
// FIXME: this is now sorted (use transpose!)
// FIXME: tested, works fine ...
val clonalPool = c.germinate

// let's print out some of the errors

var poolFitness: IndexedSeq[Double] = (0 until clonalPool.length).map { i =>
  val fitness = getScore(clonalPool(i), target)
  val sumOfAbsMean = fitness.map(Math.abs).sum / fitness.length
  println(s"Fitness: $sumOfAbsMean")
  sumOfAbsMean
}

def getFitness(idx: Int): Double = poolFitness(idx)

// this sorts the sucker
val fitnessMap = poolFitness.zipWithIndex.map { case (u,v) => (v,u) }.toMap

import scala.collection.immutable.ListMap
ListMap(fitnessMap.toSeq.sortBy(_._2):_*)
// a bunch of these can be found @ Alvin's!
// http://alvinalexander.com/scala/
// how-to-sort-map-in-scala-key-value-sortby-sortwith

// TODO: put all of the above into one, confusing, illegible, statement ...
// TODO: actually, no, that's horrific. anyway, onwards ...

// TODO: yep, lovely stuff.
c.getClonePoolFitness(getFitness)
