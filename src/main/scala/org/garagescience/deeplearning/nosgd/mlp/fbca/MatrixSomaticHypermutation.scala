package org.garagescience.deeplearning.nosgd.mlp.fbca

import org.apache.spark.ml.linalg.{Matrices, Matrix}
import org.garagescience.deeplearning.nosgd.{Double2BitStringConvert, Hypermutate}

import scala.collection.immutable.{Seq => ISeq}

// specialised trait to operate on matrices. can be applied to e.g. weight
// matrix from a nueral network

// TODO: could abstract some of these out into a trait
class MatrixSomaticHypermutation(weights: Matrix,
                                f: ISeq[ISeq[Double]] => ISeq[ISeq[Double]],
                                 val poolSize: Int = 20) extends Hypermutate {

  import Double2BitStringConvert._
  import org.garagescience.deeplearning.nosgd.Matrix2BinarySeq._

  // clones represents a list of the matrix of weights which will
  // persist as we constantly mutate the weights
  var clones: List[ISeq[ISeq[Double]]] = createClones(weights.toSeqOfSeq)(f)

  /**
    *
    * @param xsw the matrix in sequence-of-sequence form
    * @param f   a function which will initialise the matrix appropriately
    * @return    a clonal pool of initialised matrices
    */
  def createClones(xsw: ISeq[ISeq[Double]])
                     (f: ISeq[ISeq[Double]] => ISeq[ISeq[Double]]): List[ISeq[ISeq[Double]]] =
    { for {i <- 0 to poolSize}  yield f(xsw) }.toList


  /**
    * Creates the next clonal pool from a list of best candidate matrices
    * @param lm List of best performing mutant matrices
    */
  def newClonalPool(lm: List[Matrix]): Unit = {
    clones = lm.map(e=>e.toSeqOfSeq)
  }


  /**
    * encode takes a sequence of double & returns a sequence of but strings
    * representing the doubles
    *
    * @param xs sequences of doubles
    * @return   sequence of string buffers representing the doubles
    */
  def encode(xs: ISeq[Double]): ISeq[StringBuffer] =
    xs.map(d => new StringBuffer(toBinaryString(d)))

  /**
    * sequenceEncode takes a seq-of-seq of doubles representing a matrix
    * @param loc sequence of sequence of doubles
    * @return    sequence of sequence of binary strings
    */
  def sequenceEncode(loc: ISeq[ISeq[Double]]): ISeq[ISeq[StringBuffer]] =  loc map encode

  // TODO: okay. we go through the clonal pool, apply som. hype
  // TODO: then we need to return a bunch of matrices one-by-one
  // TODO: to be evaluated

  /**
    *
    * @param xsw
    * @return
    */
  protected def _germinate(xsw: ISeq[ISeq[Double]]): List[ISeq[ISeq[Double]]] =
    { for {i <- 0 until poolSize}
      yield sequenceEncode(xsw).
        map(xs => xs.map(sb => somaticHypermutation(sb)))
    }.toList


  /**
    *
    * @return
    */
  def germinate: List[List[Matrix]] =
    clones.map(xxs => _germinate(xxs)).
      map(lxsw => toMatrix(lxsw))



  /**
    * toMatrix takes a list of super-mutated clones & returns a list of
    * matrices constructed from them
    *
    * @param lxsw list of seq-of-seq of super-mutated clones of the original
    *             clones to be evaluated
    * @return     list of matrices
    */
  def toMatrix(lxsw: List[ISeq[ISeq[Double]]]): List[Matrix] =
    lxsw.map(xsw => Matrices.dense(
      xsw.length,
      xsw(0).length,
      { xsw.flatMap(xs => xs).toArray } ).transpose)



}