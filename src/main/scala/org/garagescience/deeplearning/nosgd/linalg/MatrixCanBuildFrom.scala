package org.garagescience.deeplearning.nosgd.linalg

import scala.collection.generic.CanBuildFrom

/**
 * Matrix-specific base trait for builder factories.
 *
 * @param <From> the type of the underlying collection that requests a builder to be created
 * @param <Elem> the element type of the collection to be created
 * @param <To> the type of the collection to be created
 * @since 2.0.0
 * @author h2b
 */
trait MatrixCanBuildFrom [-From, Elem, +To] extends CanBuildFrom[From, Elem, To] {

  def apply (from: From): MatrixBuilder[Elem, To]

  def apply (): MatrixBuilder[Elem, To]

}