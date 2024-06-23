package otfs

import scala.math.{Pi, cos, sin}
import breeze.linalg.{DenseMatrix, DenseVector, max}
import scala.annotation.targetName
import breeze.math.Complex

type ComplexVector = DenseVector[Complex]
type ComplexMatrix = DenseMatrix[Complex]

case class ComplexSVD(u: ComplexMatrix, singularValues: DenseVector[Double], vT: ComplexMatrix)

extension (x: Double) {
  @targetName("createComplexFromDoubles")
  def >-<(y: Double): Complex = Complex(x, y)
}

extension (matrix: ComplexMatrix) {
  // A complex-valued matrix can be seen as a matrix of matrices since each complex number can be represented as a 2x2 real matrix
  // A complex number conjugate is the same operation as transpose of real-valued 2x2 matrix which represents that complex number
  // Thus, a hermitian of complex-valued matrix is the same as transpose of expanded real-valued matrix which represents it
  def hermitian: ComplexMatrix = matrix.t.map(_.conjugate)

  def normalized: ComplexMatrix = {
    val spectralNorm = max(svd.singularValues)
    matrix.map(_ / spectralNorm)
  }

  def svd: ComplexSVD = {
    // A matrix of complex values is a larger matrix of real values
    // So to take an SVD of complex matrix we need to expand it first
    val realPart = matrix.map(_.real)
    val imagPart = matrix.map(_.imag)

    val combined = DenseMatrix.horzcat(
      DenseMatrix.vertcat(realPart, imagPart),
      DenseMatrix.vertcat(-imagPart, realPart)
    )

    val breeze.linalg.svd.SVD(u, s, vT) = breeze.linalg.svd(combined)
    val uComplex = DenseMatrix.zeros[Complex](matrix.rows, matrix.cols)
    val vTComplex = DenseMatrix.zeros[Complex](matrix.rows, matrix.cols)

    for (i <- 0 until matrix.rows) {
      for (j <- 0 until matrix.cols) {
        uComplex(i, j) = u(i, j) >-< u(i + matrix.rows, j)
        vTComplex(i, j) = vT(i, j) >-< vT(i + matrix.cols, j)
      }
    }

    ComplexSVD(uComplex, s(0 until matrix.rows), vTComplex)
  }
}

object Tools {
  val speedOfLight: Long = 299792458L // m/s

  def dftMatrix(dimensionality: Int): ComplexMatrix = {
    // This will give a Discrete Fourier Transform matrix for vectors of given dimensionality
    // When multiplied with such a vector it will decompose it into orthogonal frequency components
    val fundamentalFrequency = 2.0 * Pi / dimensionality

    val data = for {
      k <- 0 until dimensionality
      n <- 0 until dimensionality
      c = cos(fundamentalFrequency * k * n)
      s = -sin(fundamentalFrequency * k * n)
    } yield c.round >-< s.round

    new DenseMatrix(dimensionality, dimensionality, data.toArray)
  }
}
