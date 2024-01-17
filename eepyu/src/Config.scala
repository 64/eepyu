package eepyu

import spinal.core._
import spinal.core.sim._
import spinal.core.formal._

object Config {
  def spinal = SpinalConfig(
    targetDirectory = "out",
    defaultConfigForClockDomains = ClockDomainConfig(
      resetKind = BOOT
    )
  )

  def sim: SpinalSimConfig = {
    var config = SimConfig.withConfig(spinal.includeSimulation).withVcdWave

    val simOpts = sys.env.getOrElse("EEPYU_SIM_OPTS", "")
    if (simOpts.contains("optimize")) {
      config = config.allOptimisation
    }
    if (simOpts.contains("iverilog")) {
      config = config.withIVerilog
    }
    if (simOpts.contains("verilator")) {
      config = config.withVerilator
    }

    config
  }

  def synth = Config.spinal.includeSynthesis

  def formal = FormalConfig.withConfig(spinal.includeFormal.includeSimulation)
}
