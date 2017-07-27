package org.garagescience.deeplearning.nosgd.bca

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

// TODO: so let's implement a Seq to hold the previous vals 