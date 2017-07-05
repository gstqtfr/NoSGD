package org.garagescience.deeplearning.nosgd.linalg

import scala.collection.mutable.{ ArrayBuffer, Builder }
import scala.reflect.ClassTag

// import de.h2b.scala.lib.math.linalg.Vector.At
import org.garagescience.deeplearning.nosgd.linalg.Vector.At

/**
 * Base trait of vector builders.
 *
 * @param <E> the type of the vector elements
 * @param <V> the type of resulting vector
 * @author h2b
 */
trait VectorBuilder [E, +V] extends Builder[E, V] {

  protected var startIndex = 1;

  protected val elems = ArrayBuffer.empty[E]

  def at (index: Int): this.type = {startIndex = index; this}

  def += (x: E): this.type = {elems += x; this}

  def clear () = elems.clear()

  def result (): V

  /**
   * Sets the element with the specified index to the given value.
   * Widens the concrete index range if necessary,
   *
   * @param i the index
   * @param x the value
   */
  def update [F <: E : ClassTag] (i: Int, x: F): Unit = {
    val elemsIndex = i-startIndex
    val zero = Vector.scal0[F]
    if (elemsIndex<0) {
      startIndex = i
      ArrayBuffer.fill(-elemsIndex-1)(zero) ++=: elems
      x +=: elems
    } else if (elemsIndex>=elems.length) {
      elems ++= ArrayBuffer.fill(elemsIndex-elems.length)(zero)
      elems += x
    } else {
      elems(elemsIndex) = x
    }
  }

}

object VectorBuilder {

	/**
	 * @return a default vector builder
	 */
	def apply [E : ClassTag] (): VectorBuilder[E, Vector[E]] = new DefaultVectorBuilder[E]

}

class DefaultVectorBuilder [E : ClassTag] private[linalg] extends VectorBuilder[E, Vector[E]] {

  def result () = VectorFactory(At(startIndex), elems)

}
