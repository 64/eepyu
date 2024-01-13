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

  def sim(args: Array[String]): SpinalSimConfig = {
    var config = SimConfig.withConfig(spinal.includeSimulation).withVcdWave

    if (args.contains("--optimise")) {
      config = config.allOptimisation
    }

    if (args.contains("--iverilog")) {
      config = config.withIVerilog
    }

    config
  }

  def synth = Config.spinal.includeSynthesis

  def formal = FormalConfig.withConfig(spinal.includeFormal)
}
