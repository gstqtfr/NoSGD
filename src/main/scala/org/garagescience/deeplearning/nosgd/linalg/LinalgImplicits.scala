package org.garagescience.deeplearning.nosgd.linalg

import breeze.linalg.{DenseVector => DVector, Vector => BVector}
import breeze.linalg.{DenseMatrix => DMatrix, Vector => BMatrix}
import org.garagescience.deeplearning.nosgd.linalg.Vector.At

import scala.reflect.ClassTag

// TODO: need the matrix version
// TODO: DAH!!! this is FUKD!!! come back & fix ...


/*
TODO: usage:

val nativeDVector: DVector[Double] = DVector(Array(0.0, 0.0, 0.0): _*)
// => nativeDVector: breeze.linalg.DenseVector[Double] = DenseVector(0.0, 0.0, 0.0)



val nativeLVector: Vector[Double] = Vector.at(0)(Array(1.0, 2.0, 3.0): _*)
// nativeLVector: org.garagescience.deeplearning.nosgd.linalg.Vector[Double] = (1.0,2.0,3.0)@0

 */


object LinalgImplicits {

  implicit class LBVector2LVector[T: ClassTag](val v: BVector[T]) {

    def fromDenseVector2LinalgVector: Vector[T] = Vector.at(0)(v.toArray: _*)

    def fromLinalgVector2DenseVector[T](_v: Vector[T])(implicit c: ClassTag[T]): DVector[T] =
      DVector(_v.toArray: _*)
  }

  implicit class LBMatrix2LMatrix[T: ClassTag](val m: DMatrix[T]) {

    def toMatrix[T]: Matrix[T] = ???
  }
}