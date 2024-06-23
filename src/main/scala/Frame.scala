package otfs

import breeze.linalg.DenseVector
import scodec.bits.{BitVector, ByteVector}

case class FrameParams(dopplerBins: Int, delayBins: Int, freqDelta: Double, freqCarrier: Double) {
  // Normalizing the DFT matrix ensures that transformation preserves the energy of the signal,
  // preventing any scaling effects that could distort the signal's amplitude
  val dftNorm: ComplexMatrix = Tools.dftMatrix(dopplerBins).normalized

  // Ensures orthogonality of OTFS basis functions
  // Similar to minimizing Sinc interference in OFDM
  val blockDuration: Double = 1D / freqDelta

  val dopplerResolution: Double = 1D / (dopplerBins * blockDuration)

  val delayResolution: Double = 1D / (delayBins * freqDelta)

  // Total number of information symbols (like QAM)
  val totalSymbols: Int = dopplerBins * delayBins
}

case class Frame(params: FrameParams, qam: QAM) {
  // Total number of bits transferred in each OTFS frame
  val bitsPerFrame: Long = params.totalSymbols * qam.order.bits

  def generate(message: ByteVector): Iterator[ComplexVector] = {
    // A sequence of delay-Doppler space matrices flattened into vectors
    val paddingSize = (bitsPerFrame - message.size * 8 % bitsPerFrame) % bitsPerFrame
    val paddedBits = message.bits ++ BitVector.fill(n = paddingSize)(high = false)
    qam.modulate(paddedBits).grouped(params.totalSymbols).map(DenseVector.apply)
  }
}