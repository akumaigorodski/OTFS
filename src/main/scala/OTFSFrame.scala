package otfs

import breeze.linalg.{DenseMatrix, DenseVector}
import scodec.bits.{BitVector, ByteVector}
import breeze.math.Complex

// N = dopplerBins, M = delayBins
case class FrameParams(dopplerBins: Int, delayBins: Int, freqDelta: Double, freqCarrier: Double) {
  // Normalizing the DFT matrix ensures that transformation preserves the energy of the signal,
  // preventing any scaling effects that could distort the signal's amplitude
  val dftNorm: ComplexMatrix = Tools.dftMatrix(dopplerBins).normalized

  val permute: ComplexMatrix = Tools.permutationMatrix(dopplerBins, delayBins)

  val id: ComplexMatrix = DenseMatrix.eye[Complex](delayBins)

  // Ensures orthogonality of OTFS basis functions
  // Similar to minimizing Sinc interference in OFDM
  val blockDuration: Double = 1D / freqDelta

  val dopplerResolution: Double = 1D / (dopplerBins * blockDuration)

  val delayResolution: Double = 1D / (delayBins * freqDelta)

  // Total number of information symbols (like QAM)
  val totalSymbols: Int = dopplerBins * delayBins
}

case class OTFSFrame(params: FrameParams, qam: QAM) {
  // Total number of bits transferred in each OTFS frame
  val bitsPerFrame: Long = params.totalSymbols * qam.order.bits

  def generate(message: ByteVector): List[ComplexVector] = {
    val paddingSize = (bitsPerFrame - message.size * 8 % bitsPerFrame) % bitsPerFrame
    val paddedBits = message.bits ++ BitVector.fill(n = paddingSize)(high = false)

    qam.modulate(paddedBits).grouped(params.totalSymbols).map { array =>
      val X = new DenseMatrix(params.delayBins, params.dopplerBins, array)
      DenseVector(X.t.toArray)
    }.toList
  }

  def modulate(message: ByteVector): Seq[ComplexVector] = {
    val modulator = params.permute * Tools.kron(params.id, params.dftNorm.t)
    generate(message).map(modulator.*)
  }
}