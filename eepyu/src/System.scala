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

  io.uart.txd := False

  // val uartCtrl = UartCtrl(config =
  //   UartCtrlInitConfig(
  //     baudrate = 115200,
  //     dataLength = 7, // 8 bits
  //     parity = UartParityType.NONE,
  //     stop = UartStopType.ONE
  //   )
  // )
  // uartCtrl.io.uart <> io.uart

  val core = new Core()
  val mem = new Memory(
    core.imemWidth,
    core.memWidth,
    Seq(
      // Blinky
      // addi x1, x0, 0
      // addi x2, x0, 1
      // slli x2, x2, 20
      //
      // loop:
      // addi x1, x1, 1
      // and x3, x1, x2
      // sltiu x3, x3, 1
      // sw x3, 4(x0)
      // jal x0, loop
      0x00000093L, 0x00100113L, 0x01411113L, 0x00108093L, 0x0020f1b3L, 0x0011b193L, 0x00302223L, 0xff1ff06fL
    )
  )

  io.redLeds(0) := False
  io.redLeds(1) := False
  io.redLeds(2) := False
  // io.redLeds(3) := core.io.error

  mem.io <> core.io.mem

  def whenWriteAddr(x: Int) = {
    core.io.mem.memAddr === x && core.io.mem.memEnable && core.io.mem.memWriteEnable
  }
  // io.redLeds(2) := RegNextWhen(core.io.mem.memWriteData(0), whenWriteAddr(0)) init False
  io.redLeds(3) := RegNextWhen(core.io.mem.memWriteData(0), whenWriteAddr(4)) init False

  GenerationFlags.synthesis {
    io.greenLed := True
  }
  GenerationFlags.simulation {
    io.greenLed := False
  }

  // uartCtrl.io.write <> uartCtrl.io.read
}

object SystemVerilog extends App {
  Config.synth.generateVerilog(ClockDomain.external("", frequency = FixedFrequency(12 MHz))(new System))
}
