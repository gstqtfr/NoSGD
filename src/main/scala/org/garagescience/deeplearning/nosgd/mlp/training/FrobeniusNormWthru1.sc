import org.jblas.DoubleMatrix

// need a random matrix generator here ...

// val m1 = new DoubleMatrix.rand(3,3)

val m1 = DoubleMatrix.rand(3,3)
val m2 = DoubleMatrix.rand(3,3)

m1.sub(m2)

// yep, a distance measure, nifty ...

m1.sub(m1).norm2

// == 0.0!!! nifty.
// so we can minimise ...