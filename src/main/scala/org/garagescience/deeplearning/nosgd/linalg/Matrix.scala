package org.garagescience.deeplearning.nosgd.linalg

import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

/**
 * An immutable algebraic matrix consisting of {@code E} elements.
 *
 * The index range of a matrix in both dimensions spans the whole {@code Int}
 * set. The concrete range is {@code
 * [(index.dim1.low,index.dim1.high),(index.dim2.low,index.dim2.high)]};
 * all values outside of the concrete range are treated as zero. An
 * {@code IndexOutOfBoundsException} will never occur.
 *
 * @author jkk
 *
 * @param E the element type
 */
trait Matrix [E] extends Iterable[Vector[E]] with MatrixLike[E, Matrix[E]] with MatrixStore[E]
  with Equals with Immutable {

  /**
   * @return the number of rows of this matrix
   */
  def height = index.dim1.size

  /**
   * @return the number of columns of this matrix
   */
  def width = index.dim2.size

  /**
   * @return if this is a square matrix (regarding to the concrete index range)
   */
  def isSquare = height==width

  /**
   * @return if this matrix contains no elements other than scal0
   */
  def isZero = find(!_.isZero)==None

  /**
   * Returns the vector of row sums of this matrix.
   *
   * @return the vector of row sums of '''this'''
   */
  def rowSum (): Vector[E] = {
    val one = Matrix.scal1[E]
    val u = Vector((i: Int) => one, index.dim2.low, index.dim2.high)
    this*u
  }

  /**
   * Returns the vector of column sums of this matrix.
   *
   * @return the vector of column sums of '''this'''
   */
  def colSum (): Vector[E] = transposed().rowSum()

  def iterator = rowIterator

	def rowIterator = new Iterator[Vector[E]] {

	  private var i = index.dim1.low

		def hasNext = i<=index.dim1.high

		def next = {
		  if (!hasNext) throw new NoSuchElementException("iterator overflow")
	    i+=1
	    row(i-1)
	  }

  }

	def colIterator = new Iterator[Vector[E]] {

	  private var j = index.dim2.low

		def hasNext = j<=index.dim2.high

		def next = {
		  if (!hasNext) throw new NoSuchElementException("iterator overflow")
	    j+=1
	    col(j-1)
	  }

  }

	def elemIterator = new Iterator[E] {

	  private var k = 0

	  def hasNext = k<index.longSize

	  def next = {
		  if (!hasNext) throw new NoSuchElementException("iterator overflow")
	    val ij = index(k)
	    k += 1
	    apply(ij._1, ij._2)
	  }

	}

  override protected[this] def newBuilder: MatrixBuilder[Vector[E], Matrix[E]] = Matrix.newBuilder

  override def toString = mkString("(", ",", ")@"+index.dim1.low+","+index.dim2.low)

  override def canEqual (other: Any) = other.isInstanceOf[Matrix[E]]

  override def equals (other: Any) =
    other match {
    case that: Matrix[E] => that.canEqual(this) && this.hashCode==that.hashCode
    case _ => false
  }

  /**
   * Checks if this matrix is similar to the other one in terms of stripping by
   * leading and trailing zero elements in both dimensions, respectively.
   *
   * @param other
   * @return true if both matrices have the same elements in equal index
   * positions, disregarding the concrete index ranges; false otherwise
   *
   * @since 2.1.0
   */
  def ~~ (other: Matrix[E]): Boolean =
    (this.isZero && other.isZero) || this.shorten==other.shorten

}

object Matrix {

  class AtRow (val index: Int)

  object AtRow {
	  implicit val Low = AtRow(1)
		def apply(i: Int) = new AtRow(i)
  }

  def fromSeq [E : ClassTag] (seq: Seq[Vector[E]]) (low: AtRow) = MatrixFactory(low.index, seq)

  /**
   * @return a matrix with specified row vectors and implicitly given lower
   * row-index bound.
   * @note Use this factory method when either the default implicit lower index
   * bound At(1) should be applied or you want to define your own implicit At
   * object in scope. If the low argument would be stated explicitly, a ClassTag
   * argument had to be specified as well, so in this case it is better to use
   * the {atRow} method with explicit index parameter.
   * @example val a = Matrix(Vector(11,12,13), Vector(21,22,23)) //uses default implicit At(1)
   * @example implicit val Low = At(0); val a = Matrix.atRow(0)(Vector(11,12,13), Vector(21,22,23)) //uses specified implicit At(0)
   */
  def apply [E : ClassTag] (elems: Vector[E]*) (implicit low: AtRow) = fromSeq(elems)(low)

  /**
   * @return a matrix with specified row vectors and lower row-index bound
   * @example val a = Matrix.atRow(0)(Vector(11,12,13), Vector(21,22,23))
   * @note the column-index bounds are derived from the row vectors
   */
  def atRow [E : ClassTag] (at: Int) (elems: Vector[E]*) = fromSeq(elems)(AtRow(at))

  /**
   * @return a matrix with row vectors computed from the specified function in
   * the index range `[begin, end]`.
   * @example val a = Matrix((i: Int) => Vector(i*10+1,i*10+2,i*10+3), 1, 3)
   */
  def apply [E : ClassTag] (f: Int => Vector[E], begin: Int, end: Int) = {
    val builder = newBuilder[E] at begin
    for (i <- begin to end) builder += f(i)
    builder.result()
  }

  def newBuilder [E : ClassTag]: MatrixBuilder[Vector[E], Matrix[E]] = MatrixBuilder[E]()

  implicit def canBuildFrom [E : ClassTag]: MatrixCanBuildFrom[Matrix[_], Vector[E], Matrix[E]] =
    new MatrixCanBuildFrom[Matrix[_], Vector[E], Matrix[E]] {
	    def apply (): MatrixBuilder[Vector[E], Matrix[E]] = newBuilder[E]
      def apply (from: Matrix[_]): MatrixBuilder[Vector[E], Matrix[E]] = newBuilder[E] at from.index.dim1.low
  }

  /**
   * @return the scalar zero according to E
   */
  def scal0 [E : ClassTag]: E = Vector.scal0[E]

  /**
   * @return the scalar one according to E
   */
  def scal1 [E : ClassTag]: E = Vector.scal1[E]

}