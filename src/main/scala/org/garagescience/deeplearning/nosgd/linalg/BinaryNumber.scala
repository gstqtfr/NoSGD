package org.garagescience.deeplearning.nosgd.linalg

import scala.language.implicitConversions
import scala.language.higherKinds

sealed trait BinaryNumber

final object Zero extends BinaryNumber

final object One extends BinaryNumber

// companion to build binary numbers
object BinaryNumber {

  // here's the question: do we wrap in an Option? or do
  // we throw an exception (yuk!)?
  // N.B. the default case is *very* cheeky
  implicit def int2Binary(n: Int): BinaryNumber = n match {
    case 0 => Zero
    case 1 => One
    case _ => Zero
  }

  implicit def long2Binary(l: Long): BinaryNumber = l match {
    case 0 => Zero
    case 1 => One
    case _ => Zero
  }

  implicit def char2Binary(c: Char): BinaryNumber = c match {

    case '0' => Zero
    case '1' => One
    case _ => Zero
  }

}







// FIXME: the problem below is that we have to construct the list (or whatever)
// FIXME: on the R.H.S., can't go from a pre-existing list ...
// usage: val x: List[BinaryNumber] = List(1, 0, 1)
// => x: List[BinaryNumber] = List(One$@3c3a0032, Zero$@54e02f6a, Zero$@54e02f6a)
// val x: scala.collection.immutable.Vector[BinaryNumber] = scala.collection.immutable.Vector(0,1,1,1,0,0,1,0)
// val x: Seq[BinaryNumber] = Seq(0,1,1,1,0,0,1,0)
// works a treat - nifty ...

/*
object BinarySequence {

  import scala.collection.generic.CanBuildFrom

  implicit def toBinaryNumber[A: ClassTag, C[A] <: Traversable[A]](as: C[A])
                                                                  (implicit cbf: CanBuildFrom[C[A],
                                                                    A,
                                                                    C[A]]): C[A] = {

    as.toArray.to[C]
  }
}
*/


