package otfs

import scodec.bits.*
import breeze.math.Complex
import breeze.numerics.log2
import scala.collection.mutable.ArrayBuffer

// Convolutional code is characterized by (N = output bits, K = input bits, M = number of registers)
// For TCM we need (N = QAM bits, K = QAM bits - 1, M = design choice, we choose it to always be 2)
type ConvolutionalPolynomial = Seq[BitVector]

enum ModulationOrder(tcmM2Poly: ConvolutionalPolynomial, val value: Int) {
  case QAM4 extends ModulationOrder(Seq(bin"111", bin"101"), math.pow(2, 2).toInt) // (N2, K1, M3)
  case QAM16 extends ModulationOrder(Seq(bin"111", bin"101", bin"110", bin"100"), math.pow(2, 4).toInt) // (N4, K3, M3)
  case QAM64 extends ModulationOrder(Seq(bin"111", bin"101", bin"110", bin"100", bin"011", bin"010"), math.pow(2, 6).toInt) // (N6, K5, M3)
  case QAM256 extends ModulationOrder(Seq(bin"111", bin"101", bin"110", bin"100", bin"011", bin"010", bin"001", bin"000"), math.pow(2, 8).toInt) // (N8, K7, M3)

  // This is M(=2) parameter of Convolutional Code
  val numOfRegisters: Int = tcmM2Poly.head.size.toInt - 1
  // This is number of bits encoded by QAM
  val bits: Int = log2(value).toInt

  def encode(data: BitVector): BitVector = {
    val initState = (BitVector.empty, BitVector low numOfRegisters)
    val (encoded, _) = data.grouped(bits - 1).foldLeft(initState) { case (Tuple2(acc, state), dataBits) =>
      val state1 = state.shiftLeft(dataBits.size).or(dataBits).takeRight(numOfRegisters + dataBits.size)
      val out = tcmM2Poly.map(poly => (state1 & poly).populationCount % 2 == 1)
      (acc ++ BitVector.bits(out), state1)
    }

    encoded
  }

  def decode(data: BitVector): BitVector = {
    val trellisLength = data.size.toInt / bits
    val numStates = 1 << numOfRegisters
    val inputBits = bits - 1

    val pathMetrics = Array.fill(trellisLength + 1, numStates)(Double.PositiveInfinity)
    val survivorPaths = Array.fill(trellisLength + 1, numStates)(0)
    val numInputCombinations = 1 << inputBits
    pathMetrics(0)(0) = 0.0

    for {
      step <- 1 to trellisLength
      receivedBits = data.slice(bits * (step - 1), bits * step)

      currentState <- 0 until numStates
      inputCombination <- 0 until numInputCombinations
    } {
      val previousState = (currentState >> inputBits) | ((inputCombination & ((1 << numOfRegisters) - 1)) << (numOfRegisters - inputBits))

      if (previousState >= 0 && previousState < numStates) {
        val stateBits = BitVector.fromInt(previousState, numOfRegisters)
        val encodedBits = tcmM2Poly.map(poly => (stateBits & poly).populationCount % 2 == 1)
        val metric = encodedBits.zipWithIndex.count { case (bit, idx) => receivedBits(idx) != bit }
        val newMetric = pathMetrics(step - 1)(previousState) + metric

        if (newMetric < pathMetrics(step)(currentState)) {
          survivorPaths(step)(currentState) = previousState
          pathMetrics(step)(currentState) = newMetric
        }
      }
    }

    // Traceback
    val shift = (1 << inputBits) - 1
    val decodedBits = new ArrayBuffer[Boolean]
    var (_, state) = pathMetrics(trellisLength).zipWithIndex.minBy(_._1)

    for (step <- trellisLength to 1 by -1) {
      val inputBitsValue = survivorPaths(step)(state) & shift
      decodedBits ++= BitVector.fromInt(inputBitsValue, inputBits).toIndexedSeq
      state = survivorPaths(step)(state) >> inputBits
    }

    BitVector.bits(decodedBits.reverse.take(data.size.toInt / bits * (bits - 1)))
  }
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
