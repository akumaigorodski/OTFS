package otfs

import scodec.bits.*
import breeze.numerics.log2
import breeze.math.Complex

enum ModulationOrder(val value: Int) {
  case QAM4 extends ModulationOrder(math.pow(2, 2).toInt)
  case QAM16 extends ModulationOrder(math.pow(2, 4).toInt)
  case QAM64 extends ModulationOrder(math.pow(2, 6).toInt)
  case QAM256 extends ModulationOrder(math.pow(2, 8).toInt)
  case QAM1024 extends ModulationOrder(math.pow(2, 10).toInt)
  val bits: Int = log2(value).toInt
}

class QuadAmpMod(order: ModulationOrder) {
  private def grayCode(n: Int): Int = n ^ (n >> 1)

  private val constellation: Seq[Complex] = {
    val sqrt = math.sqrt(order.value).toInt

    val realParts = for {
      real <- 0 until sqrt
      updated = 2 * real - sqrt + 1
    } yield updated.toDouble

    for {
      real <- realParts
      imaginary <- realParts.reverse
    } yield Complex(real, imaginary)
  }

  val qamSymbolToBits: Map[Complex, BitVector] =
    constellation.zipWithIndex.map { case (point, index) =>
      point -> BitVector.fromInt(grayCode(index), order.bits)
    }.toMap

  val bitsToQamSymbol: Map[BitVector, Complex] =
    qamSymbolToBits.map(_.swap)

  def modulate(data: BitVector): Seq[Complex] = {
    val paddingSize = order.bits - data.size % order.bits
    val paddedBits = data ++ BitVector.fill(paddingSize)(false)
    paddedBits.grouped(order.bits).map(bitsToQamSymbol).toSeq
  }

  def demodulate(symbols: Seq[Complex] = Nil): ByteVector =
    symbols.map(qamSymbolToBits).foldLeft(BitVector.empty)(_ ++ _).bytes
}
