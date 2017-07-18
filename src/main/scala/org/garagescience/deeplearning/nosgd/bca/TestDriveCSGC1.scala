import org.garagescience.deeplearning.nosgd.bca._

object TestDriveCSGC1 {



  def creatTarget(d: Double): Array[Double] = {
    for (i <- 0 until 10) yield d
  }.toArray



  def getScore(xs1: Array[Double], xs2: Array[Double]): Array[Double] =
    xs1.zip(xs2).map { case (u,v) => u - v }




  def main(args: Array[String]): Unit = {

    val candidate: Array[Double] = {
      for (i <- 0 until 20) yield scala.util.Random.nextGaussian
    }.toArray

    val target = creatTarget(Math.sqrt(2))

    val c = new ConstrainedSequenceHypermutation(candidate)


    (0 until 50000).foreach { iteration =>
      // the current clonal pool
      val clonalPool = c.clones

      // get the fitness of the clones
      val currentFitness: Array[Double] = (0 until clonalPool.length).map { i =>
        getScore(clonalPool(i), target).map(Math.abs).sum
      }.toArray

      // next generation
      val newClonalPool: Array[Array[Double]] = c.germinate

      // next-gen. fitness
      val newFitness: Array[Double] = (0 until newClonalPool.length).map { i =>
        getScore(newClonalPool(i), target).map(Math.abs).sum
      }.toArray

      // update the clonal pool with hypermutated new clones
      c.update(currentFitness, newFitness, newClonalPool)

      // TODO: be nice to have these sorted ...

      (0 until newFitness.length).map { idx =>
        println(s"$iteration $idx ${currentFitness(idx)} ${newFitness(idx)}")
      }
      println()

    }


  }

}