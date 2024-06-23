package otfs

import scodec.bits.ByteVector

@main
def main(): Unit = {
  val encoding4 = QAM(ModulationOrder.QAM4)
  val params = FrameParams(dopplerBins = 2, delayBins = 4, freqDelta = 15e3, freqCarrier = 4e9)
  val message = ByteVector.encodeUtf8("Hello, world!").getOrElse(ByteVector.empty)
  println(Frame(params, encoding4).generate(message).toList)
}