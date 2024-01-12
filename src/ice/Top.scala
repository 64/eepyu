package ice

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._

case class Core() extends Component {
  val io = new Bundle {
    val uart = master(Uart())

    val greenLed = out Bool ()
    val redLeds = out Vec (Bool(), 4)
  }

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
  io.redLeds(3) := False

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

case class Top(pllFreq: Option[FixedFrequency]) extends Component {
  val io = new Bundle {
    val uart = master(Uart())

    val greenLed = out Bool ()
    val redLeds = out Vec (Bool(), 4)
  }

  GenerationFlags.synthesis {
    if (pllFreq.isEmpty) {
      val core = Core()
      core.io <> io
    } else {
      val clkCtrl = new Area {
        val coreClockDomain = ClockDomain.internal(
          name = "core",
          frequency = pllFreq.get,
          config = ClockDomainConfig(resetKind = SYNC)
        )

        val pll = new IcePll(coreClockDomain)

        pll.io.clock_in := clockDomain.readClockWire
        coreClockDomain.clock := pll.io.clock_out
        coreClockDomain.reset := ResetCtrl.asyncAssertSyncDeassert(
          input = !pll.io.locked,
          clockDomain = coreClockDomain
        )
      }

      val coreArea = new ClockingArea(clkCtrl.coreClockDomain) {
        val core = Core()
        core.io <> io
      }
    }
  }

  GenerationFlags.simulation {
    val core = Core()
    core.io <> io
  }
}

object ClockedTop {
  val externalClockFreq = FixedFrequency(12 MHz)
  val pllFreq = FixedFrequency(60 MHz)

  def apply() = ClockDomain.external("", frequency = externalClockFreq)(Top(Some(pllFreq)))
}

object TopVerilog extends App {
  Config.spinal.includeSynthesis.generateVerilog(ClockedTop()).mergeRTLSource("BlackboxRTL")
}
