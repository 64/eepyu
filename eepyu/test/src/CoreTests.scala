package eepyu

import spinal.core._
import spinal.core.sim._

import org.scalatest.funsuite._
import com.carlosedp.riscvassembler.RISCVAssembler
import spinal.lib.misc.pipeline._

class CoreTests extends AnyFunSuite {
  def assemble(inst: String) = BigInt(RISCVAssembler.binOutput(inst), 2)

  def withAssembledProgramMemory(dut: Core, imem: Seq[String]) = {
    withProgramMemory(dut, imem.map(assemble))
  }

  def withProgramMemory(dut: Core, imem: Seq[BigInt]) = {
    dut.clockDomain.forkStimulus(2)
    dut.io.mem.memReadAddr #= 0

    val imemThread = fork {
      while (true) {
        dut.clockDomain.waitSampling(1)
        val addr = dut.io.mem.imemReadAddr.toInt / 4
        val data = imem.lift(addr).getOrElse(BigInt(0))
        dut.io.mem.imemReadData #= data
      }
    }

    // val retireMonitor = fork {
    //   while (true) {
    //     dut.clockDomain.waitSamplingWhere(dut.io.rvfi_valid.toBoolean)
    //     val inst = dut.io.rvfi_insn.toBigInt
    //     // println(f"Retired instruction $inst%x")
    //   }
    // }
  }

  def nextRetiredInstruction(dut: Core) = {
    dut.clockDomain.waitSamplingWhere(dut.io.rvfi_valid.toBoolean)
    val inst = dut.io.rvfi_insn.toBigInt
    println(f"Retired instruction $inst%x")
    inst
  }

  val compiled = Config.sim.compile(new Core(8, 8))

  test("should trap on illegal instruction") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.io.mem.imemReadData #= 0
      dut.io.mem.memReadData #= 0
      assert(!dut.io.error.toBoolean)
      dut.clockDomain.waitSampling(5)
      sleep(0)
      assert(dut.io.error.toBoolean)
    }
  }

  test("should not trap on add instructions") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.io.mem.imemReadData #= 0
      dut.io.mem.memReadData #= 0

      assert(!dut.io.error.toBoolean)
      dut.clockDomain.waitSampling(1)

      dut.io.mem.imemReadData #= assemble("add x0, x1, x2")
      dut.io.mem.memReadData #= 0

      for (i <- 0 until 10) {
        assert(!dut.io.error.toBoolean)
        dut.clockDomain.waitSampling(1)
      }
    }
  }

  test("should writeback to registers") {
    val program =
      Seq(
        "addi x1, x0, 1",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0"
      )
    compiled.doSim { dut =>
      withAssembledProgramMemory(dut, program)

      for (i <- 0 until 6) {
        assert(dut.regFile.rs1mem.getBigInt(1) == 0)
        dut.clockDomain.waitSampling(1)
      }

      for (i <- 0 until 5) {
        assert(dut.regFile.rs1mem.getBigInt(1) == 1)
        dut.clockDomain.waitSampling(1)
      }
    }
  }

  test("should retire instructions in order") {
    val program =
      Seq(
        "addi x0, x0, 0",
        "addi x0, x0, 1",
        "addi x0, x0, 2",
        "addi x0, x0, 3",
        "addi x0, x0, 4",
        "addi x0, x0, 5",
        "addi x0, x0, 6",
        "addi x0, x0, 7"
      )
    compiled.doSim { dut =>
      withAssembledProgramMemory(dut, program)
      assert(nextRetiredInstruction(dut) == assemble(program(0)))
      assert(nextRetiredInstruction(dut) == assemble(program(1)))
      assert(nextRetiredInstruction(dut) == assemble(program(2)))
      assert(nextRetiredInstruction(dut) == assemble(program(3)))
      assert(nextRetiredInstruction(dut) == assemble(program(4)))
      assert(nextRetiredInstruction(dut) == assemble(program(5)))
      assert(nextRetiredInstruction(dut) == assemble(program(6)))
      assert(nextRetiredInstruction(dut) == assemble(program(7)))
    }
  }

  test("should jump") {
    val program =
      Seq(
        "addi x1, x0, 1",
        "sub x0, x0, x0",
        "jal x2, -8",
        "xori x0, x0, 0",
        "xori x0, x0, 0"
      )
    compiled.doSim { dut =>
      withAssembledProgramMemory(dut, program)

      assert(nextRetiredInstruction(dut) == assemble(program(0)))
      assert(nextRetiredInstruction(dut) == assemble(program(1)))
      assert(nextRetiredInstruction(dut) == assemble(program(2)))

      assert(nextRetiredInstruction(dut) == assemble(program(0)))
      assert(nextRetiredInstruction(dut) == assemble(program(1)))
      assert(nextRetiredInstruction(dut) == assemble(program(2)))
    }
  }

  test("should branch") {
    val program =
      Seq(
        "addi x1, x0, 1",
        "addi x2, x0, 3",
        "addi x1, x1, 1",
        "blt x1, x2, -4",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0"
      )
    compiled.doSim { dut =>
      withAssembledProgramMemory(dut, program)

      assert(nextRetiredInstruction(dut) == assemble(program(0)))
      assert(nextRetiredInstruction(dut) == assemble(program(1)))
      assert(nextRetiredInstruction(dut) == assemble(program(2)))
      // assert(nextRetiredInstruction(dut) == assemble(program(3)))

      // assert(nextRetiredInstruction(dut) == assemble(program(2)))
      // assert(nextRetiredInstruction(dut) == assemble(program(3)))
      //
      // assert(nextRetiredInstruction(dut) == assemble(program(4)))
      // assert(nextRetiredInstruction(dut) == assemble(program(5)))
      // assert(nextRetiredInstruction(dut) == assemble(program(6)))

      nextRetiredInstruction(dut)
      nextRetiredInstruction(dut)
      nextRetiredInstruction(dut)
    }
  }

  test("hazard") {
    val program =
      Seq(
        "addi x1, x0, 5",
        "addi x2, x0, 3",
        "sub x3, x1, x2",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0"
      )
    compiled.doSim { dut =>
      withAssembledProgramMemory(dut, program)
      dut.clockDomain.waitSampling(10)

      assert(dut.regFile.rs1mem.getBigInt(1) == 5)
      assert(dut.regFile.rs1mem.getBigInt(2) == 3)
      assert(dut.regFile.rs1mem.getBigInt(3) == 2)
    }
  }

  test("rv32ui-p-add") {}
}
