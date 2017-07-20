/**
 *
 */
package org.garagescience.deeplearning.nosgd.linalg

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

/**
 * An immutable algebraic vector consisting of {@code E} elements.
 *
 * The index range of a vector spans the whole {@code Int} set. The concrete
 * range is {@code [index.low, index.high]}; all values outside of the concrete
 * range are treated as zero. An {@code IndexOutOfBoundsException} will never
 * occur.
 *
 * @author h2b
 *
 * @param [E] the element type
 */
trait Vector [E] extends Iterable[E]
  with VectorLike[E, Vector[E]]
  with VectorStore[E] with Equals with Immutable {

  /**
   * @return the length of this vector
   */
  def length = index.size

  /**
   * @return if this vector contains no elements other than scal0
   */
  def isZero = {
    val scal0 = Vector.scal0[E]
    find(_!=scal0)==None
  }

  /**
   * Returns the scalar product of '''this''' and `v`.
   *
   * @param v
   * @return '''this'''*`v`
   */
  def * (v: Vector[E]): E = {
    val index = this.index intersect v.index
    var s = Vector.scal0
    for (i <- index) {
      val t = op.times(this(i), v(i))
      s = op.plus(s, t)
    }
    s
  }

  /**
   * Returns the Euklidian norm of this vector.
   *
   * @return ||'''this'''||
   */
  def norm: Double

	def iterator = new Iterator[E] {

	  private var i = index.low

	  def hasNext = i<=index.high

	  def next = {
		  if (!hasNext) throw new NoSuchElementException("iterator overflow")
		  i+=1
		  apply(i-1)
	  }

  }

  override protected[this] def newBuilder: VectorBuilder[E, Vector[E]] = Vector.newBuilder

  override def toString = mkString("(", ",", ")@"+index.low)

  override def canEqual (other: Any) = other.isInstanceOf[Vector[E]]

  override def equals (other: Any) =
    other match {
    case that: Vector[E] => that.canEqual(this) && this.hashCode==that.hashCode
    case _ => false
  }

  /**
   * Checks if this vector is similar to the other one in terms of stripping by
   * leading and trailing zero elements.
   *
   * @param other
   * @return true if both vectors have the same elements in equal index
   * positions, disregarding the concrete index ranges; false otherwise
   *
   * @since 2.1.0
   */
  def ~~ (other: Vector[E]): Boolean =
    (this.isZero && other.isZero) || this.shorten==other.shorten

}

object Vector {

  class At (val index: Int)

  // TODO: may have to hack this ...
  object At {
	  implicit val Low = At(1)
		def apply(i: Int) = new At(i)
  }

  def fromSeq [E : ClassTag] (seq: Seq[E]) (low: At) = VectorFactory(low, seq)

  /**
   * @return a vector with specified elements and implicitly given lower index
   * bound.
   * @note Use this factory method when either the default implicit lower index
   * bound At(1) should be applied or you wnat to define your own implicit At
   * object in scope. If the low argument would be stated explicitly, a ClassTag
   * argument had to be specified as well, so in this case it is better to use
   * the {@code at} method with explicit index parameter.
   * @example val v = Vector(1,2,3) //uses default implicit At(1)
   * @example implicit val low = At(0); val v = Vector(1,2,3) //uses specified implicit At(0)
   */
  def apply [E : ClassTag] (elems: E*) (implicit low: At) = fromSeq(elems)(low)

  /**
   * @return a vector with specified elements and lower index bound.
   * @example val v = Vector.at(0)(1,2,3)
   */
  def at [E : ClassTag] (at: Int) (elems: E*) = fromSeq(elems)(At(at))

  /**
   * @return a vector with elements computed from the specified function in the
   * index range `[begin, end]`.
   * @example val v = Vector((i: Int) => i*i: Double, 1, 5)
   */
  def apply [E : ClassTag] (f: Int => E, begin: Int, end: Int) = {
    val builder = newBuilder[E] at begin
    for (i <- begin to end) builder += f(i)
    builder.result()
  }

  def newBuilder [E : ClassTag]: VectorBuilder[E, Vector[E]] = VectorBuilder[E]()

  implicit def canBuildFrom [E : ClassTag]: VectorCanBuildFrom[Vector[_], E, Vector[E]] =
    new VectorCanBuildFrom[Vector[_], E, Vector[E]] {
	    def apply (): VectorBuilder[E, Vector[E]] = newBuilder[E]
      def apply (from: Vector[_]): VectorBuilder[E, Vector[E]] = newBuilder[E] at from.index.low
  }

  /**
   * @return the scalar zero according to E
   */
  def scal0 [E : ClassTag]: E = VectorFactory.zero[E]

  /**
   * @return the scalar one according to E
   */
  def scal1 [E : ClassTag]: E = VectorFactory.one[E]

}