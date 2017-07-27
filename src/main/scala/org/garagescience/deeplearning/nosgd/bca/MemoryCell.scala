package org.garagescience.deeplearning.nosgd.bca

import scala.language.higherKinds
import scala.language.implicitConversions


// TODO: we need a memoery cell to provide the momentum function
// TODO: this simply stores the previous selected candidate from
// TODO: the last evaluation & adds it in proportion to a coeff.

// TODO: this style of thing:

/*

// TODO: this is from:
// https://stats.stackexchange.com/questions/70101/neural-networks-weight-change-momentum-and-weight-decay

Momentum αα is used to diminish the fluctuations in weight changes over consecutive iterations:

Δωi(t+1)=−η∂E∂wi+αΔωi(t),
Δωi(t+1)=−η∂E∂wi+αΔωi(t),
where E(w)E(w) is the error function, ww - the vector of weights, ηη - learning rate.
 */

// TODO: so let's implement a type-parameterised collection to hold the previous vals

trait Memory[M[_], +A] {

  def alpha: A
  def memory[M[A]]
  // def memory[U]: M[U]

}
