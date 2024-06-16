package otfs

import breeze.math.Complex
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector
import ModulationOrder._

class QAMSpec extends AnyFunSuite {
  val encoding16: QuadAmpMod = QuadAmpMod(QAM16)

  test("Constellation mapping") {
    val qamSymbol: Complex = Complex(3.0, 3.0)
    assert(encoding16.qamSymbolToBits(qamSymbol).toBin == "1010")
  }

  test("Mod/Demod") {
    val message = "hello"
    val data = ByteVector.encodeUtf8(message).getOrElse(ByteVector.empty)
    val qamSymbols = encoding16.modulate(data.bits)
    val result = encoding16.demodulate(qamSymbols)
    assert(result.decodeUtf8 contains message)
  }
}
