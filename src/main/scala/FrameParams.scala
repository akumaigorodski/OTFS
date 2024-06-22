package otfs

case class FrameParams(dopplerBins: Int, delayBins: Int, freqDelta: Double, freqCarrier: Double) {
  val dftNorm: ComplexMatrix = Tools.dftMatrix(dopplerBins).normalized

  val blockDuration: Double = 1D / freqDelta

  val dopplerResolution: Double = 1D / (dopplerBins * blockDuration)

  val delayResolution: Double = 1D / (delayBins * freqDelta)
}
