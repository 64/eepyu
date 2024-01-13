package eepyu

import spinal.core._

class IcePll(pllClockDomain: ClockDomain) extends BlackBox {
  val io = new Bundle {
    val clock_in = in Bool()
    val clock_out = out Bool()
    val locked = out Bool()
  }

  noIoPrefix()
  addRTLPath("./src/eepyu/blackbox/IcePll.v")
}
