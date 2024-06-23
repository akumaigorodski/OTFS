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

case class QAM(order: ModulationOrder) {
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
    } yield real >-< imaginary
  }

  val qamSymbolToBits: Map[Complex, BitVector] =
    constellation.zipWithIndex.map { case (point, index) =>
      point -> BitVector.fromInt(grayCode(index), order.bits)
    }.toMap

  val bitsToQamSymbol: Map[BitVector, Complex] =
    qamSymbolToBits.map(_.swap)

  def modulate(data: BitVector): Array[Complex] = 
    data.grouped(order.bits).map(bitsToQamSymbol).toArray

  def demodulate(symbols: Seq[Complex] = Nil): BitVector =
    symbols.map(qamSymbolToBits).foldLeft(BitVector.empty)(_ ++ _)
}
