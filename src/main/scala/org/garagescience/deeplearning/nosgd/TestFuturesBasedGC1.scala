package org.garagescience.deeplearning.nosgd

import org.apache.spark.ml.linalg.{Matrices, Matrix}

import scala.collection.immutable.Seq
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

// notes on Promise & Future

// To complete a Promise with a success, you call its success method, passing it the value
// that the Future associated with it is supposed to have:

// taxcut.success(TaxCut(20))


// this style of thing:

/*

val doComplete: PartialFunction[Try[String],Unit] = {                // <3>
  case s @ Success(_) => println(s)                                  // <4>
  case f @ Failure(_) => println(f)
}

// usage:

val futures = (0 to 9) map {                                         // <5>
  case i if i % 2 == 0 => Future.successful(i.toString)
  case i => Future.failed(ThatsOdd(i))
}
futures map (_ onComplete doComplete)
 */


object TestFuturesBasedGC1 {

  import org.garagescience.deeplearning.nosgd.mlp.MathUtils._

  private final val iterations = 500
  private final val popSize = 50
  private final val poolSize = 20

  // easier problem, we can see whether the bloody thing
  // *works* with this
  private final val target = Matrices.dense(3, 3,
    Array(
      0.0, 0.0, 0.0,
      0.0, 0.0, 0.0,
      0.0, 0.0, 0.0
    )
  )

  private def randomMatrix(rows: Int, cols: Int, sz: Int): Matrix = {
    val tmpArray = for {i <- 0 until sz} yield scala.util.Random.nextGaussian
    Matrices.dense(rows, cols, tmpArray.toArray)
  }

  // TODO: error functions: these'll need futurising, too ...
  private def error1(m1: Matrix): Double = (target - m1).abs.toArray.sum

  private def error2(m1: Matrix): Double = (target - m1).abs.toArray.sum

  private def error(m1: Matrix): Double = {
    val f = Future(
      Math.sqrt(
        Math.pow(Math.abs((for {i <- 0 until target.numRows
                                j <- 0 until target.numCols}
          yield target(i, j) - m1(i, j)).sum), 2.0)
      )
    )
    Await.result(f, Duration.Inf)
  }

  private def getRMSE(xs: Seq[Double])(implicit ec: ExecutionContext): Double =
      Math.sqrt(Math.pow(Math.abs(xs.sum / popSize), 2.0))

  /*
  def sequence[A](list: List[Future[A]])
                 (implicit ec: ExecutionContext): Future[List[A]] = {
    val seed = Future.successful(List.empty[A])
    list.foldLeft(seed)((acc,f) => for (l <- acc; a <- f) yield a :: l)
      .map(_.reverse)
  }
  */

  private def getUpdate(m: MatrixGerminalCentre)
                       (f: Matrix => Double)
                       (implicit ec: ExecutionContext): Seq[Double] = {
    val future = Future(m.update(f))
    Await.result(future, Duration.Inf)
  }


  def main(args: Array[String]): Unit = {

    val mgc: Seq[MatrixGerminalCentre] = for {i <- 0 until popSize}
      yield new MatrixGerminalCentre(randomMatrix(
        target.numRows,
        target.numCols,
        target.toArray.length
      ), poolSize)

    // TODO: Futures/Promises
    for (i <- 0 until iterations) {

      mgc.map((gc: MatrixGerminalCentre) =>
        getUpdate(m = gc)(f = error)).foreach { result => println(s"Result: $result") }

      /*
      val errors: Seq[Seq[Future[Double]]] = mgc.map(gc => gc.clones).
        map(xs => xs.map(d => error(d)))
      */


      /*
      mgc.map(gc => gc.clones).
        map(xs => xs.map(d => error(d))).foreach { f =>
        f.onComplete { result => println(s"Result: $result") }

      }
     */


      val doComplete: PartialFunction[Try[Double], Unit] = {
        case s@Success(_) => println(s)
        case f@Failure(_) => println(f)
      }


      val getSeq:PartialFunction[Try[Seq[Double]], Seq[Double]] = {
        case s @ Success(_) => s.get
        case f @ Failure(_) => Seq(0.0)
      }


      // futures map (_ onComplete doComplete)

      /*
      def getRMSE(fxs: Seq[Future[Double]])(implicit ec: ExecutionContext): Future[Double] = {
        val xs: Seq[PartialFunction[Try[Seq[Double]], Seq[Double]]] = fxs.map(e => getSeq)
        // Future(Math.sqrt(Math.pow(Math.abs(xs.sum / popSize), 2.0)))

      }
      */

      //val errorFutures: Seq[Seq[Future[Double]]] = mgc.map(gc => gc.clones).
      //  map(xs => xs.map((d: Matrix) => error(d)))


      // TODO: this needs to be inverted - we want a Future[Seq[Double]]!!!

      val errors: Seq[Seq[Double]] = mgc.
        map((gc: MatrixGerminalCentre) => gc.clones).
        map((xs: Seq[Matrix]) => xs.map(error(_)))



      //errorFutures map (_ onComplete doComplete)




      /*
            errors.foreach { xs => xs.foreach { f =>
              f.onComplete { _try => _try match {
                case Success(result) => println(s"$i ${getRMSE(result)}")
              }
              }
              }
            }
      */

      errors.foreach(xs => println(s"$i ${getRMSE(xs)}"))
      println()

      // TODO: this needs to be future'd
      //  mgc.map(gc => gc.update(error))


      // TODO: this sort of thig:
      // def timesFourInParallel(n: Int)(implicit ec: ExecutionContext): Future[Int] =
      //     Future.sequence(timesTwo(n) :: timesTwo(n) :: Nil).map(_.sum)


      /*
      // TODO: this style of thing ...
      // fa.flatMap(a => fb.map(b => a + b))
       */


    }

  }


}
