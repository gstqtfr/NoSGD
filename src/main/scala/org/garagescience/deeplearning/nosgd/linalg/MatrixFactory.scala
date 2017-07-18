package org.garagescience.deeplearning.nosgd.linalg

import scala.reflect.ClassTag

/**
 * @author h2b
 */
trait MatrixFactory {

  def create [E : ClassTag] (rowStart: Int, elems: Seq[Vector[E]]): Matrix[E]

}

object MatrixFactory {

  def apply [E : ClassTag] (lowRow: Int, seq: Seq[Vector[E]]) =
	  RowMatrixFactory.create(lowRow, seq)

}