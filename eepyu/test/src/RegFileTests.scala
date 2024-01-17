package eepyu

import spinal.core._
import spinal.core.sim._
import spinal.core.formal._

import org.scalatest.funsuite._

import scala.util.Random

class RegFileTests extends AnyFunSuite {
  val compiled = Config.sim.compile(new RegFile)

  test("should read zero for the zero register") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.rd #= 0
      dut.io.rdData #= 0
      dut.io.writeValid #= false

      dut.io.rs1 #= 0
      dut.io.rs2 #= 0
      dut.clockDomain.waitSampling()
      sleep(0)
      assert(dut.io.rs1Data.toLong == 0 && dut.io.rs2Data.toLong == 0)
    }
  }

  test("should ignore writes to the zero register") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.rd #= 0
      dut.io.rdData #= 1
      dut.io.writeValid #= true

      dut.io.rs1 #= 0
      dut.io.rs2 #= 0
      dut.clockDomain.waitSampling()
      sleep(0)
      assert(dut.io.rs1Data.toLong == 0)
    }
  }

  test("should apply writes to normal registers") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.rd #= 1
      dut.io.rdData #= 1
      dut.io.writeValid #= true

      dut.io.rs1 #= 1
      dut.io.rs2 #= 0

      // Check read-under-write behaviour is correct
      dut.clockDomain.waitSampling()
      sleep(0)
      assert(dut.io.rs1Data.toLong == 0)

      dut.clockDomain.waitSampling()
      sleep(0)
      assert(dut.io.rs1Data.toLong == 1)
    }
  }
}
