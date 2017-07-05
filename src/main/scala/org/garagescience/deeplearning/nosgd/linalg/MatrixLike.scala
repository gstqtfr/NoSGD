package org.garagescience.deeplearning.nosgd.linalg

import scala.collection.IterableLike
import scala.reflect.ClassTag

/**
 * Implementation trait for matrices of type `Matrix[E}`.
 *
 * @param <E> type of the matrix elements
 * @param <M> type of resulting matrix
 * @since 2.0.0
 * @author h2b
 */
trait MatrixLike [E, +M <: Matrix[E]] extends IterableLike[Vector[E], M]  {

  override protected[this] def thisCollection: Matrix[E] = this.asInstanceOf[Matrix[E]]
  override protected[this] def toCollection(repr: M): Matrix[E] = repr.asInstanceOf[Matrix[E]]

  override protected[this] def newBuilder: MatrixBuilder[Vector[E], M]

  protected implicit val elemTag: ClassTag[E]

	/**
	 * @param at the lower row-index bound of the new matrix
	 * @return a copy of this matrix with the specified lower row-index bound
	 */
	def atRow (at: Int): M = {
    val builder = newBuilder at at
    for (elem ← thisCollection) builder += elem
    builder.result()
	}

	/**
	 * @param at the lower column-index bound of the new matrix
	 * @return a copy of this matrix with the specified lower column-index bound;
	 * all row vectors are shifted to this bound
	 */
	def atCol (at: Int): M = {
	  val coll = thisCollection
    val builder = newBuilder at coll.index.dim1.low
    for (elem ← coll) builder += elem @@ at
    builder.result()
	}

	/**
	 * @param at tuple of the lower row- and column-index bounds of the new matrix
	 * @return a copy of this matrix with the specified lower row- and
	 * column-index bounds; all row vectors are shifted to this bound
	 */
	def @@ (at: (Int, Int)): M = {
	  (thisCollection atRow at._1 atCol at._2).asInstanceOf[M]
	}

  /**
   * @return this matrix with a shortened index range stripped by leading and
   * trailing zero-vector elements after shortening the vector elements
   * itself (i.e., making concrete leading and trailing zeroes virtual in
   * both dimensions)
   *
   * @since 2.1.0
   */
	def shorten: M = {
	  val zerovec = Vector[E]()
	  val coll = thisCollection map { (x: Vector[E]) => x.shorten }
	  val index = coll.index.dim1
    val low = index.find(!coll(_).isZero).getOrElse(Index.Maxdex)
    val high = index.reverse.find(!coll(_).isZero).getOrElse(Index.Mindex)
    val builder = newBuilder at low
    for (i ← low to high) builder += coll(i)
    builder.result()
	}

  /**
   * @param index the requested index ranges in both dimensions (the actual
   * index ranges will be the union of this argument and the existing ones)
   * @return this vector with widened index ranges extended by leading and
   * trailing zero-vector elements according to `index.dim1` and then widening
   * the vector elements itself according to `index.dim2` (i.e., in both
   * dimensions adding concrete leading and trailing zeroes that were virtual
   * before)
   *
   * @since 2.1.0
   */
  def widen (index: Index2): M = {
	  val zerovec = Vector[E]().widen(index.dim2)
    val coll = thisCollection map { (x: Vector[E]) => x.widen(index.dim2) }
    val collIndex = coll.index.dim1
    val builder = newBuilder at (index.dim1.low min collIndex.low)
    for (i ← index.dim1.low until collIndex.low) builder += zerovec
    for (i ← collIndex.low to collIndex.high) builder += coll(i)
    for (i ← collIndex.high+1 to index.dim1.high) builder += zerovec
    builder.result()
  }

  /**
   * Returns identity. Can also be used to copy '''this'''.
   *
   * @return +'''this'''
   */
  def unary_+ (): M = unaryOp((x: Vector[E]) ⇒ x)

  private def unaryOp (f: Vector[E] ⇒ Vector[E]): M = {
    val coll = thisCollection
    val builder = newBuilder at coll.index.dim1.low
    for (elem ← coll) builder += f(elem)
    builder.result()
  }

  /**
   * Returns negative complement of '''this'''.
   *
   * @return -'''this'''
   */
  def unary_- (): M = unaryOp((x: Vector[E]) ⇒ -x)

  /**
   * Scales this matrix by `s`.
   *
   * @param s
   * @return '''this'''*`s`
   */
  def * (s: E): M = unaryOp((x: Vector[E]) ⇒ x*s)

  /**
   * Returns the transpose of this matrix..
   *
   * @return '''this'''^T^
   */
  def transposed (): M = {
	  val coll = thisCollection
    val builder = newBuilder at coll.index.dim2.low
    for (i <- coll.index.dim2) builder += coll.col(i)
    builder.result()
  }

  /**
   * Returns the sum of '''this''' and `b`.
   *
   * @param b the other matrix to be added
   * @return '''this'''+`b`
   */
  def + [That >: M <: Matrix[E]] (b: That)
      (implicit vbf: VectorCanBuildFrom[Vector[E], E, Vector[E]], mbf: MatrixCanBuildFrom[this.type, Vector[E], That]): That =
    binaryOp((x: Vector[E], y: Vector[E]) ⇒ x+y, b)

  private def binaryOp [That >: M <: Matrix[E]] (f: (Vector[E], Vector[E]) ⇒ Vector[E], b: That)
      (implicit bf: MatrixCanBuildFrom[this.type, Vector[E], That]): That = {
    val a = thisCollection
    val index1 = a.index.dim1 union b.index.dim1
    val builder = bf() at index1.low
    for (i <- index1) builder += f(a(i), b(i))
    builder.result()
  }

  /**
   * Returns the difference of '''this''' and `b`.
   *
   * @param b the other matrix to be subtracted
   * @return  '''this'''-`b`
   */
  def - [That >: M <: Matrix[E]] (b: That)
      (implicit vbf: VectorCanBuildFrom[Vector[E], E, Vector[E]], mbf: MatrixCanBuildFrom[this.type, Vector[E], That]): That =
    binaryOp((x: Vector[E], y: Vector[E]) ⇒ x-y, b)

  /**
   * Returns the product of '''this''' and `b`.
   *
   * @param b the other matrix to be multiplied
   * @return '''this'''*`b`
   */
  def * [That >: M <: Matrix[E]] (b: That)
      (implicit vbf: VectorCanBuildFrom[Vector[E], E, Vector[E]], mbf: MatrixCanBuildFrom[this.type, Vector[E], That]): That = {
    val a = thisCollection
    val mbuilder = mbf() at a.index.dim1.low
    for (i <- a.index.dim1) {
      val ai = a.row(i)
      val vbuilder = vbf() at b.index.dim2.low
      for (j <- b.index.dim2) vbuilder += ai*b.col(j)
      mbuilder += vbuilder.result()
    }
    mbuilder.result()
  }

  /**
   * Returns the product of '''this''' and `v`.
   *
   * @param v the vector to be multiplied
   * @return '''this'''*`v`
   */
  def * [That <: Vector[E]] (v: That) (implicit vbf: VectorCanBuildFrom[That, E, That]): That = {
    val a = thisCollection
    val vbuilder = vbf() at a.index.dim2.low
    for (row  <- a.rowIterator) vbuilder += row*v
    vbuilder.result
  }

}