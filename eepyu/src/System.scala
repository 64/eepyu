package eepyu

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._

class SystemIO extends Bundle {
  val uart = master(Uart())

  val greenLed = out Bool ()
  val redLeds = out Vec (Bool(), 4)
}

case class System() extends Component {
  val io = new SystemIO

  val uartCtrl = UartCtrl(config =
    UartCtrlInitConfig(
      baudrate = 115200,
      dataLength = 7, // 8 bits
      parity = UartParityType.NONE,
      stop = UartStopType.ONE
    )
  )
  uartCtrl.io.uart <> io.uart

  io.redLeds(0) := False
  io.redLeds(1) := False
  io.redLeds(2) := False
  // io.redLeds(3) := False
  // io.greenLed := False

  // val aluUart = new util.AluUart
  // aluUart.io.data << uartCtrl.io.read.asFlow.as(Flow(UInt(8 bits)))
  // io.redLeds(3) := aluUart.io.led

  // val decoder = new Decoder
  // decoder.io.inst := 0
  //
  // val shift = Reg(UInt(32 bits))
  // val count = Counter(4)
  // when (uartCtrl.io.read.fire) {
  //   shift := (shift |<< 8) | uartCtrl.io.read.payload.asUInt.resized
  //   count.increment()
  // }
  //
  // when(count === 0) {
  //   decoder.io.inst := shift
  //
  //   io.redLeds(0) := decoder.io.rType
  //   io.redLeds(1) := decoder.io.iType
  //   io.redLeds(2) := decoder.io.sType
  //   io.redLeds(3) := decoder.io.jType
  //   io.greenLed := decoder.io.bType
  // }

  val core = new Core()
  val mem = new Memory(core.imemWidth, core.memWidth, Seq(19, 4292866159L))
  mem.io <> core.io.mem
  io.redLeds(3) := core.io.error

  GenerationFlags.synthesis {
    io.greenLed := True
  }
  GenerationFlags.simulation {
    io.greenLed := False
  }

  uartCtrl.io.write <> uartCtrl.io.read
}

object SystemVerilog extends App {
  Config.synth.generateVerilog(ClockDomain.external("", frequency = FixedFrequency(12 MHz))(new System))
}
