package otfs

import scodec.bits.*
import breeze.numerics.log2
import breeze.math.Complex

class QuadAmpMod(order: Int) {
  private val permitted = (3 to 10).map(exp => math.pow(2, exp).toInt).toList
  require(permitted.contains(order), "Modulation order must be 2^[3, 10]")

  private def grayCode(n: Int): Int = n ^ (n >> 1)

  val bits: Int = log2(order).toInt

  val constellation: Seq[Complex] = {
    val sqrtM = math.sqrt(order).toInt

    val realParts = for {
      real <- 0 until sqrtM
      updated = 2 * real - sqrtM + 1
    } yield updated.toDouble

    for {
      real <- realParts
      imag <- realParts.reverse
    } yield Complex(real, imag)
  }

  val qamSymbolToBits: Map[Complex, BitVector] =
    constellation.zipWithIndex.map { case (point, index) =>
      point -> BitVector.fromInt(grayCode(index), bits)
    }.toMap

  val bitsToQamSymbol: Map[BitVector, Complex] =
    qamSymbolToBits.map(_.swap)

  def modulate(data: BitVector): Seq[Complex] = {
    val paddingSize = (bits - data.size % bits) % bits
    val paddedBits = data ++ BitVector.fill(paddingSize)(false)
    paddedBits.grouped(this.bits).map(bitsToQamSymbol).toSeq
  }

  def demodulate(symbols: Seq[Complex] = Nil): ByteVector =
    symbols.map(qamSymbolToBits).foldLeft(BitVector.empty)(_ ++ _).bytes
}