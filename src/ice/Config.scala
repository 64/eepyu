package ice

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
    var conf = SimConfig.withConfig(spinal.includeSimulation).withFstWave

    if (args.contains("--optimise")) {
      conf = conf.allOptimisation
    }

    if (args.contains("--iverilog")) {
      conf = conf.withIVerilog
    }

    conf
  }

  def formal = FormalConfig.withConfig(spinal.includeFormal)
}
