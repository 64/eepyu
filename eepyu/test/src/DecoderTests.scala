package eepyu

import spinal.core._
import spinal.core.sim._

import org.scalatest.funsuite._
import com.carlosedp.riscvassembler.RISCVAssembler

class DecoderTests extends AnyFunSuite {
  def assemble(inst: String) = BigInt(RISCVAssembler.binOutput(inst), 2)

  def checkRType(decoder: Decoder, inst: String) = {
    decoder.io.inst #= assemble(inst)
    sleep(1)
    assert(!decoder.io.error.toBoolean)
    assert(decoder.io.rType.toBoolean)
    assert(!decoder.io.iType.toBoolean)
    assert(!decoder.io.sType.toBoolean)
    assert(!decoder.io.jType.toBoolean)
    assert(!decoder.io.bType.toBoolean)
  }

  def checkIType(decoder: Decoder, inst: String) = {
    decoder.io.inst #= assemble(inst)
    sleep(1)
    assert(!decoder.io.error.toBoolean)
    assert(!decoder.io.rType.toBoolean)
    assert(decoder.io.iType.toBoolean)
    assert(!decoder.io.sType.toBoolean)
    assert(!decoder.io.jType.toBoolean)
    assert(!decoder.io.bType.toBoolean)
  }

  def checkSType(decoder: Decoder, inst: String) = {
    decoder.io.inst #= assemble(inst)
    sleep(1)
    assert(!decoder.io.error.toBoolean)
    assert(!decoder.io.rType.toBoolean)
    assert(!decoder.io.iType.toBoolean)
    assert(decoder.io.sType.toBoolean)
    assert(!decoder.io.jType.toBoolean)
    assert(!decoder.io.bType.toBoolean)
  }

  def checkJType(decoder: Decoder, inst: String) = {
    decoder.io.inst #= assemble(inst)
    sleep(1)
    assert(!decoder.io.error.toBoolean)
    assert(!decoder.io.rType.toBoolean)
    assert(!decoder.io.iType.toBoolean)
    assert(!decoder.io.sType.toBoolean)
    assert(decoder.io.jType.toBoolean)
    assert(!decoder.io.bType.toBoolean)
  }

  def checkBType(decoder: Decoder, inst: String) = {
    decoder.io.inst #= assemble(inst)
    sleep(1)
    assert(!decoder.io.error.toBoolean)
    assert(!decoder.io.rType.toBoolean)
    assert(!decoder.io.iType.toBoolean)
    assert(!decoder.io.sType.toBoolean)
    assert(!decoder.io.jType.toBoolean)
    assert(decoder.io.bType.toBoolean)
  }

  val compiled = Config.sim.compile(new Decoder)

  test("Not decode garbage") {
    compiled.doSim { dut =>
      dut.io.inst #= 0
      sleep(1)
      assert(dut.io.error.toBoolean)
    }
  }

  test("Decode ADD") {
    compiled.doSim { dut =>
      checkRType(dut, "add x0, x1, x2")
    }
  }

  test("Decode SUB") {
    compiled.doSim { dut =>
      checkRType(dut, "sub x0, x1, x2")
    }
  }

  test("Decode SLL") {
    compiled.doSim { dut =>
      checkRType(dut, "sll x0, x1, x2")
    }
  }

  test("Decode SLT") {
    compiled.doSim { dut =>
      checkRType(dut, "slt x0, x1, x2")
    }
  }

  test("Decode SLTU") {
    compiled.doSim { dut =>
      checkRType(dut, "sltu x0, x1, x2")
    }
  }

  test("Decode XOR") {
    compiled.doSim { dut =>
      checkRType(dut, "xor x0, x1, x2")
    }
  }

  test("Decode SRL") {
    compiled.doSim { dut =>
      checkRType(dut, "srl x0, x1, x2")
    }
  }

  test("Decode SRA") {
    compiled.doSim { dut =>
      checkRType(dut, "sra x0, x1, x2")
    }
  }

  test("Decode OR") {
    compiled.doSim { dut =>
      checkRType(dut, "or x0, x1, x2")
    }
  }

  test("Decode AND") {
    compiled.doSim { dut =>
      checkRType(dut, "and x0, x1, x2")
    }
  }

  // -------- I-type Instructions ---------

  test("Decode ADDI") {
    compiled.doSim { dut =>
      checkIType(dut, "addi x1, x2, 3")
    }
  }

  test("Decode SLLI") {
    compiled.doSim { dut =>
      checkIType(dut, "slli x0, x1, 2")
    }
  }

  test("Decode SLTI") {
    compiled.doSim { dut =>
      checkIType(dut, "slti x0, x1, 2")
    }
  }

  test("Decode SLTIU") {
    compiled.doSim { dut =>
      checkIType(dut, "sltiu x0, x1, 2")
    }
  }

  test("Decode XORI") {
    compiled.doSim { dut =>
      checkIType(dut, "xori x0, x1, 2")
    }
  }

  test("Decode SRLI") {
    compiled.doSim { dut =>
      checkIType(dut, "srli x0, x1, 2")
    }
  }

  test("Decode SRAI") {
    compiled.doSim { dut =>
      checkIType(dut, "srai x0, x1, 2")
    }
  }

  test("Decode ORI") {
    compiled.doSim { dut =>
      checkIType(dut, "ori x0, x1, 2")
    }
  }

  test("Decode ANDI") {
    compiled.doSim { dut =>
      checkIType(dut, "andi x0, x1, 2")
    }
  }

  test("Decode LB") {
    compiled.doSim { dut =>
      checkIType(dut, "lb x0, 2(x1)")
    }
  }

  test("Decode LH") {
    compiled.doSim { dut =>
      checkIType(dut, "lh x0, 2(x1)")
    }
  }

  test("Decode LW") {
    compiled.doSim { dut =>
      checkIType(dut, "lw x0, 2(x1)")
    }
  }

  test("Decode LBU") {
    compiled.doSim { dut =>
      checkIType(dut, "lbu x0, 2(x1)")
    }
  }

  test("Decode LHU") {
    compiled.doSim { dut =>
      checkIType(dut, "lhu x0, 2(x1)")
    }
  }

  test("Decode JALR") {
    compiled.doSim { dut =>
      checkIType(dut, "jalr x0, x1, 2")
    }
  }
}
