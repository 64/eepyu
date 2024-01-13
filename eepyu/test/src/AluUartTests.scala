package eepyu

import eepyu.util.AluUart

import spinal.core.sim._

import org.scalatest.funsuite._

class AluUartTests extends AnyFunSuite {
  test("AluUart") {
    Config.sim.compile(new AluUart).doSim { dut =>
      dut.io.data.valid #= false

      val clock = dut.clockDomain
      clock.forkStimulus(10)

      def txChars(dut: AluUart, chars: String) {
        clock.waitSampling()
        dut.io.data.valid #= true

        for (ch <- chars.toCharArray()) {
          dut.io.data.payload #= ch
          clock.waitSampling()
        }

        dut.io.data.valid #= false
        clock.waitSampling(2)
      }

      txChars(dut, "0")
      txChars(dut, "0000")
      txChars(dut, "0000")
      assert(!dut.io.led.toBoolean)

      txChars(dut, "0")
      txChars(dut, "0005")
      txChars(dut, "0000")
      assert(dut.io.led.toBoolean)

      txChars(dut, "0")
      txChars(dut, "0000")
      txChars(dut, "0005")
      assert(dut.io.led.toBoolean)

      txChars(dut, "0")
      txChars(dut, "0001")
      txChars(dut, "0001")
      assert(!dut.io.led.toBoolean)

      txChars(dut, "0")
      txChars(dut, "1000")
      txChars(dut, "1000")
      assert(dut.io.led.toBoolean)

      txChars(dut, "0")
      txChars(dut, "1110")
      txChars(dut, "0001")
      assert(dut.io.led.toBoolean)
    }
  }
}
