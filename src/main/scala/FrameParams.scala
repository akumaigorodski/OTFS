package otfs

case class FrameParams(dopplerBins: Int, delayBins: Int, freqDelta: Double, freqCarrier: Double) {
  // Normalizing the DFT matrix ensures that transformation preserves the energy of the signal,
  // preventing any scaling effects that could distort the signal's amplitude
  val dftNorm: ComplexMatrix = Tools.dftMatrix(dopplerBins).normalized

  // Ensures orthogonality of OTFS basis functions
  // Similar to minimizing Sinc interference in OFDM
  val blockDuration: Double = 1D / freqDelta

  val dopplerResolution: Double = 1D / (dopplerBins * blockDuration)

  val delayResolution: Double = 1D / (delayBins * freqDelta)
}
