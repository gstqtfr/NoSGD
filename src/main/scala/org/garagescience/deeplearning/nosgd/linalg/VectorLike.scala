/*
  LinAlg - Scala Library for Vector and Matrix Types and Operations

  Copyright 2015-2016 Hans-Hermann Bode

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/
package org.garagescience.deeplearning.nosgd.linalg

import scala.collection.IterableLike
import scala.reflect.ClassTag

/**
 * Implementation trait for vectors of type `Vector[E}`.
 *
 * @param <E> type of the vector elements
 * @param <V> type of resulting vector
 * @since 2.0.0
 * @author h2b
 */
trait VectorLike [E, +V <: Vector[E]] extends IterableLike[E, V] {

  override protected[this] def thisCollection: Vector[E] = this.asInstanceOf[Vector[E]]
  override protected[this] def toCollection(repr: V): Vector[E] = repr.asInstanceOf[Vector[E]]

  override protected[this] def newBuilder: VectorBuilder[E, V]

  protected implicit val elemTag: ClassTag[E]

  /**
   * Scalar arithmetics to be provided by implementation.
   */
  protected trait ScalarOps {
    def negate (x: E): E
    def plus (x: E, y: E): E
    def times (x: E, y: E): E
  }

  protected val op: ScalarOps

  /**
   * @param at the lower index bound of the new vector
   * @return a copy of this vector with the specified lower index bound
   */
  def @@ (at: Int): V = {
    val builder = newBuilder at at
    for (elem ← thisCollection) builder += elem
    builder.result()
  }

  /**
   * @return this vector with a shortened index range stripped by leading and
   * trailing zero elements (i.e., making concrete leading and trailing zeroes
   * virtual)
   *
   * @since 2.1.0
   */
  def shorten: V = {
    val scal0 = Vector.scal0[E]
    val coll = thisCollection
    val index = coll.index
    val low = index.find(coll(_)!=scal0).getOrElse(Index.Maxdex)
    val high = index.reverse.find(coll(_)!=scal0).getOrElse(Index.Mindex)
    val builder = newBuilder at low
    for (i ← low to high) builder += coll(i)
    builder.result()
  }

  /**
   * @param index the requested index range (the actual index range will
   * be the union of this argument and the existing one)
   * @return this vector with a widened index range extended by leading and
   * trailing zero elements (i.e., adding concrete leading and trailing zeroes
   * that were virtual before)
   *
   * @since 2.1.0
   */
  def widen (index: Index): V = {
    val scal0 = Vector.scal0[E]
    val coll = thisCollection
    val collIndex = coll.index
    val builder = newBuilder at (index.low min collIndex.low)
    for (i ← index.low until collIndex.low) builder += scal0
    for (i ← collIndex.low to collIndex.high) builder += coll(i)
    for (i ← collIndex.high+1 to index.high) builder += scal0
    builder.result()
  }

  /**
   * Returns identity. Can also be used to copy '''this'''.
   *
   * @return +'''this'''
   */
  def unary_+ (): V = unaryOp((x: E) ⇒ x)

  private def unaryOp (f: E ⇒ E): V = {
    val coll = thisCollection
    val builder = newBuilder at coll.index.low
    for (elem ← coll) builder += f(elem)
    builder.result()
  }

  /**
   * Returns negative complement of '''this'''.
   *
   * @return -'''this'''
   */
  def unary_- (): V = unaryOp((x: E) ⇒ op.negate(x))

  /**
   * Scales this vector by `s`.
   *
   * @param s
   * @return '''this'''*`s`
   */
  def * (s: E): V = unaryOp((x: E) ⇒ op.times(x, s))

  /**
   * Returns the sum of '''this''' and `v`.
   *
   * @param v the other vector to be added
   * @return '''this'''+`v`
   */
  def + [That >: V <: Vector[E]] (v: That) (implicit bf: VectorCanBuildFrom[this.type, E, That]): That =
    binaryOp((x: E, y: E) ⇒ op.plus(x, y), v)

  private def binaryOp [That >: V <: Vector[E]] (f: (E, E) ⇒ E, v: That) (implicit bf: VectorCanBuildFrom[this.type, E, That]): That = {
    val u = thisCollection
    val index = u.index union v.index
    val builder = bf() at index.low
    for (i <- index) builder += f(u(i), v(i))
    builder.result()
  }

  /**
   * Returns the difference of '''this''' and `v`.
   *
   * @param v the other vector to be subtracted
   * @return  '''this'''-`v`
   */
  def - [That >: V <: Vector[E]] (v: That) (implicit bf: VectorCanBuildFrom[this.type, E, That]): That =
    binaryOp((x: E, y: E) ⇒ op.plus(x, op.negate(y)), v)

}