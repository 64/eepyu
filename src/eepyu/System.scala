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

  val aluUart = new util.AluUart
  aluUart.io.data << uartCtrl.io.read.asFlow.as(Flow(UInt(8 bits)))
  io.redLeds(3) := aluUart.io.led

  GenerationFlags.synthesis {
    io.greenLed := True
  }

  GenerationFlags.simulation {
    io.greenLed := False
  }

  GenerationFlags.formal {
    io.greenLed := False
  }

  uartCtrl.io.write <> uartCtrl.io.read
}

object SystemVerilog extends App {
  Config.synth.generateVerilog(ClockDomain.external("", frequency = FixedFrequency(12 MHz))(new System))
}
