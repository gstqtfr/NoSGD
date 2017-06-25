package org.garagescience.deeplearning.nosgd

import org.apache.spark.ml.linalg.{Matrices, Matrix}
import scala.collection.immutable.Seq
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

// TODO: this is all bollox
// TODO: doesn't fucking work

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

  private def getRMSE(xs: Seq[Double])(implicit ec: ExecutionContext): Double =
    Math.sqrt(Math.pow(Math.abs(xs.sum / popSize), 2.0))

  private def error(m1: Matrix)
                   (implicit ec: ExecutionContext): Double = {
    val f = Future(
      Math.sqrt(
        Math.pow(Math.abs((for {i <- 0 until target.numRows
                                j <- 0 until target.numCols}
          yield target(i, j) - m1(i, j)).sum), 2.0)
      )
    )
    Await.result(f, Duration.Inf)
  }


  /*

val f = future {
   produceSomething()
}

val producer = future {
   continueDoingSomethingUnrelated()
}

startDoingSomething()

val consumer = future {
  f onSuccess {
    case r => doSomethingWithResult()
  }
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


  private def getUpdate(m: MatrixGerminalCentre)
                       (f: Matrix => Double)
                       (implicit ec: ExecutionContext) = {

    val future = Future {

      m.update(f)

      } onComplete {
      // here we get the error
      case s @ Success(_) =>
        val xs: Seq[Double] = s.get
        // xs.map(_m => error(_m))
    }

    // val future = Future(m.update(f))
    //Await.result(future, Duration.Inf)
    /*future.onComplete {
      case Success(xs) => xs
      case Failure(e) => Seq(0.0)
    }*/

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

      mgc.map(gc => getUpdate(m = gc)(f = error))

      Future {
        mgc.
          map((gc: MatrixGerminalCentre) => gc.clones).
          map((xs: Seq[Matrix]) => xs.map(error(_)))
      } onComplete {
        case s@Success(_) =>
          s.get.map(xs => println(s"$i ${getRMSE(xs)}"))
        case f@Failure(_) => println("ooh-er, missus ...")
      }

      //errors.foreach((xs: Seq[Double]) => println(s"$i ${getRMSE(xs)}"))

    }

    val cloneattack: Seq[Seq[Matrix]] = mgc.map(gc => gc.clones).
      map(xsm => xsm.map(xs => Matrices.dense(target.numRows, target.numRows, xs.toArray)))

    cloneattack.foreach { xs =>
      xs.foreach(m => println(s"$m\n"))
      println()
    }


  }


}
