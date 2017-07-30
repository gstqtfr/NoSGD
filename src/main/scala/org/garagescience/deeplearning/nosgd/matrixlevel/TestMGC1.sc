import org.garagescience.deeplearning.nosgd.matrixlevel._
import org.jblas.DoubleMatrix

val m = DoubleMatrix.randn(2,2)
val mgc1 = new MatrixGerminalCentre(m, 5)
mgc1.germinate
mgc1.m
