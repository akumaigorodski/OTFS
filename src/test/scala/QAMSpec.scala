package otfs

import breeze.math.Complex
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector
import ModulationOrder.*
import scodec.bits.*

class QAMSpec extends AnyFunSuite {
  val encoding16: QAM = QAM(QAM16)

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

  test("TCM Encoding/Decoding QAM16") {
    val data = BitVector(hex"1234abcd")
    val encoded = ModulationOrder.QAM4.encode(data)
    val decoded = ModulationOrder.QAM4.decode(encoded)

    println(decoded)
    assert(decoded.take(data.size) == data, "Decoded data should match the original input data.")
  }
}
