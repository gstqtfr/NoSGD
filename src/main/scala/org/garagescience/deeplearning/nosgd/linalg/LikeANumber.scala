package org.garagescience.deeplearning.nosgd.linalg

//import scala.reflect.ClassTag

//import com.github.fommil.netlib.BLAS.{getInstance => NativeBLAS}

object LikeANumber {
  trait NumberLike[@specialized (Double, Float) T] extends Serializable {
    def plus(x: T, y: T): T
    def minus(x: T, y: T): T
    def times(x: T, y: T): T
    def divide(x: T, y: Int): T
  }
  object NumberLike {
    implicit object NumberLikeDouble extends NumberLike[Double] {
      def plus(x: Double, y: Double): Double = x + y
      def minus(x: Double, y: Double): Double = x - y
      def times(x: Double, y: Double): Double = x * y
      def divide(x: Double, y: Int):  Double = x / y
    }
    implicit object NumberLikeFloat extends NumberLike[Float] {
      def plus(x: Float, y: Float): Float = x + y
      def minus(x: Float, y: Float): Float = x - y
      def times(x: Float, y: Float): Float = x * y
      def divide(x: Float, y: Int): Float = x / y
    }
  }
}
