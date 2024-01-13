package eepyu

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._

case class Top(pllFreq: FixedFrequency) extends Component {
  val io = new SystemIO

  GenerationFlags.synthesis {
    val clkCtrl = new Area {
      val coreClockDomain = ClockDomain.internal(
        name = "core",
        frequency = pllFreq,
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
      val core = System()
      core.io <> io
    }
  }

  GenerationFlags.simulation {
    val core = System()
    core.io <> io
  }
}

object ClockedTop {
  val externalClockFreq = FixedFrequency(12 MHz)
  val pllFreq = FixedFrequency(60 MHz)

  def apply() = ClockDomain.external("", frequency = externalClockFreq)(Top(pllFreq))
}

object TopVerilog extends App {
  Config.synth.generateVerilog(ClockedTop()).mergeRTLSource("BlackboxRTL")
}
