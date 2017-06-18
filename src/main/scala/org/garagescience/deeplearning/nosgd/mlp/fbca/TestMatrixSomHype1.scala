package org.garagescience.deeplearning.nosgd.mlp.fbca

import org.apache.spark.ml.linalg.{Matrices, Matrix}

import scala.collection.immutable.{Seq => ISeq}


// TODO: yeah, this is knackered. bollocks.

object TestMatrixSomHype1 {

  // this sets the initial values of the matrix
  def f(xss: ISeq[ISeq[Double]]): ISeq[ISeq[Double]] =
    xss.map(xs => xs.map(s => scala.util.Random.nextGaussian))

  def getError(m1: Matrix, m2: Matrix): Double = {
    val sumDiff = for {i <- 0 until m1.numRows
                       j <- 0 until m2.numCols}
      yield Math.abs(m1(i, j) - m2(i, j))
    sumDiff.sum
  }

  // get the best performing Matrix from the list of somhyped clones
  def getBestMutant(listOfMutants: List[List[(Double, Int)]]): List[Int] = {

    def _getBestMutant(lom: List[List[(Double, Int)]], best: List[Int]): List[Int] = lom match {
      case Nil => best
      case _head :: _tail => _getBestMutant(_tail, _head.head._2 :: best)
    }

    _getBestMutant(listOfMutants, List())
  }

  // iterate through the list of best mutants, retrieve that matrix
  def getNextClonalPool(_clones: List[List[Matrix]], _bestMutants: List[(Int, Int)]): List[Matrix] = {

    def _getNextClonalPool(__clones: List[List[Matrix]],
                           __bm: List[(Int, Int)],
                           lm: List[Matrix]): List[Matrix] = __bm match {
      case Nil => lm
      case head :: tail =>
        val best: Matrix = __clones(head._1)(head._2)
        _getNextClonalPool(__clones, tail, best :: lm)
    }

    _getNextClonalPool(_clones, _bestMutants, List())
  }

  val m1 = Matrices.dense(3, 3, Array(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0))

  val msh = new MatrixSomaticHypermutation(m1, f)

  def main(args: Array[String]): Unit = {

    for (i <- 0 until 5) {
      // we don't flatten this because we want the clonal expansion
      // for each clone
      val llm: List[List[Matrix]] = msh.germinate

      val errors: List[List[(Double, Int)]] = llm.map(lm => lm.map(m => getError(m, m1)).zipWithIndex)

      val indexedErrors: List[List[(Double, Int)]] = errors.map(l => l.sortWith {
        case (a, b) => a._1 < b._1
      })

      var rmse = 0.0
      indexedErrors.map(l => l.head).foreach { h =>
        val fitness = h._1
        val index   = h._2
        println(s"$i : $index : $fitness")
        rmse = rmse + fitness
      }
      rmse = rmse / llm.length
      println(s"RMSE: ${rmse}")
      println()



      // TODO: we need to put the best performers into the clonal pool
      // TODO: so, we should get the best (i.e. the head of each list)
      // TODO: and upload it to the germinal centre ...

      // this gets the fittest clone from the mutated pool
      val bestMutants: List[(Int, Int)] = getBestMutant(indexedErrors).
        zipWithIndex.map { case (a,b) => (b,a)}

      //println("Best mutants for this iterations")
      //bestMutants.foreach { case (i, j) => println(s"$i : $j") }

      val replacements: List[Matrix] = getNextClonalPool(llm, bestMutants)
      msh.newClonalPool(replacements)

    }
  }
}






