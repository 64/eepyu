package eepyu

import spinal.core._
import spinal.core.sim._

import org.scalatest.funsuite._
import com.carlosedp.riscvassembler.RISCVAssembler

class DecoderTests extends AnyFunSuite {
  def assemble(inst: String) = BigInt(RISCVAssembler.binOutput(inst), 2)

  def checkRType(decoder: Decoder, inst: String, aluOp: AluOp.E, rd: Int, rs1: Int, rs2: Int) = {
    decoder.io.inst #= assemble(inst)
    sleep(1)
    assert(!decoder.io.error.toBoolean)
    assert(decoder.io.rType.toBoolean)
    assert(!decoder.io.iType.toBoolean)
    assert(!decoder.io.sType.toBoolean)
    assert(!decoder.io.jType.toBoolean)
    assert(!decoder.io.bType.toBoolean)
    assert(!decoder.io.memOp.toBoolean)
    assert(decoder.io.rd.toInt == rd)
    assert(decoder.io.rs1.toInt == rs1)
    assert(decoder.io.rs2.toInt == rs2)
    assert(decoder.io.aluOp.toEnum == aluOp)
  }

  def checkIType(
      decoder: Decoder,
      inst: String,
      aluOp: AluOp.E,
      rd: Int,
      rs1: Int,
      imm: Int,
      memRead: Boolean = false,
      memMask: Option[MemType.E] = None,
      branchType: BranchType.Value = BranchType.None
  ) = {
    decoder.io.inst #= assemble(inst)
    sleep(1)
    assert(!decoder.io.error.toBoolean)
    assert(!decoder.io.rType.toBoolean)
    assert(decoder.io.iType.toBoolean)
    assert(!decoder.io.sType.toBoolean)
    assert(!decoder.io.jType.toBoolean)
    assert(!decoder.io.bType.toBoolean)
    assert(decoder.io.rd.toInt == rd)
    assert(decoder.io.rs1.toInt == rs1)
    assert(decoder.io.imm.toInt == imm)
    assert(decoder.io.aluOp.toEnum == aluOp)
    assert(decoder.io.branchType.toBoolean == (branchType == BranchType.Jump))
    assert(decoder.io.memOp.toBoolean == memRead)
    assert(!decoder.io.memWriteEnable.toBoolean)
    if (memMask.isDefined) {
      assert(decoder.io.memMask.toEnum == memMask.get)
    }
  }

  def checkSType(decoder: Decoder, inst: String, rs1: Int, rs2: Int, imm: Int, memMask: MemType.E) = {
    decoder.io.inst #= assemble(inst)
    sleep(1)
    assert(!decoder.io.error.toBoolean)
    assert(!decoder.io.rType.toBoolean)
    assert(!decoder.io.iType.toBoolean)
    assert(decoder.io.sType.toBoolean)
    assert(!decoder.io.jType.toBoolean)
    assert(!decoder.io.bType.toBoolean)
    assert(decoder.io.rs1.toInt == rs1)
    assert(decoder.io.rs2.toInt == rs2)
    assert(decoder.io.imm.toInt == imm)
    assert(decoder.io.aluOp.toEnum == AluOp.ADD)
    assert(decoder.io.memOp.toBoolean)
    assert(decoder.io.memWriteEnable.toBoolean)
    assert(decoder.io.memMask.toEnum == memMask)
  }

  def checkJType(decoder: Decoder, inst: String, rd: Int, imm: Int) = {
    decoder.io.inst #= assemble(inst)
    sleep(1)
    assert(!decoder.io.error.toBoolean)
    assert(!decoder.io.rType.toBoolean)
    assert(!decoder.io.iType.toBoolean)
    assert(!decoder.io.sType.toBoolean)
    assert(decoder.io.jType.toBoolean)
    assert(!decoder.io.bType.toBoolean)
    assert(!decoder.io.memOp.toBoolean)
    assert(decoder.io.rd.toInt == rd)
    assert(decoder.io.imm.toInt == imm)
  }

  def checkBType(decoder: Decoder, inst: String, aluOp: AluOp.E, rs1: Int, rs2: Int, imm: Int) = {
    decoder.io.inst #= assemble(inst)
    sleep(1)
    assert(!decoder.io.error.toBoolean)
    assert(!decoder.io.rType.toBoolean)
    assert(!decoder.io.iType.toBoolean)
    assert(!decoder.io.sType.toBoolean)
    assert(!decoder.io.jType.toBoolean)
    assert(decoder.io.bType.toBoolean)
    assert(!decoder.io.memOp.toBoolean)
    assert(decoder.io.rs1.toInt == rs1)
    assert(decoder.io.rs2.toInt == rs2)
    assert(decoder.io.imm.toInt == imm)
    assert(decoder.io.aluOp.toEnum == aluOp)
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
      checkRType(dut, "add x0, x1, x2", AluOp.ADD, 0, 1, 2)
    }
  }

  test("Decode SUB") {
    compiled.doSim { dut =>
      checkRType(dut, "sub x0, x1, x2", AluOp.SUB, 0, 1, 2)
    }
  }

  test("Decode SLL") {
    compiled.doSim { dut =>
      checkRType(dut, "sll x0, x1, x2", AluOp.SLL, 0, 1, 2)
    }
  }

  test("Decode SLT") {
    compiled.doSim { dut =>
      checkRType(dut, "slt x0, x1, x2", AluOp.LT, 0, 1, 2)
    }
  }

  test("Decode SLTU") {
    compiled.doSim { dut =>
      checkRType(dut, "sltu x0, x1, x2", AluOp.LTU, 0, 1, 2)
    }
  }

  test("Decode XOR") {
    compiled.doSim { dut =>
      checkRType(dut, "xor x0, x1, x2", AluOp.XOR, 0, 1, 2)
    }
  }

  test("Decode SRL") {
    compiled.doSim { dut =>
      checkRType(dut, "srl x0, x1, x2", AluOp.SRL, 0, 1, 2)
    }
  }

  test("Decode SRA") {
    compiled.doSim { dut =>
      checkRType(dut, "sra x0, x1, x2", AluOp.SRA, 0, 1, 2)
    }
  }

  test("Decode OR") {
    compiled.doSim { dut =>
      checkRType(dut, "or x0, x1, x2", AluOp.OR, 0, 1, 2)
    }
  }

  test("Decode AND") {
    compiled.doSim { dut =>
      checkRType(dut, "and x0, x1, x2", AluOp.AND, 0, 1, 2)
    }
  }

  // -------- I-type Instructions ---------

  test("Decode ADDI") {
    compiled.doSim { dut =>
      checkIType(dut, "addi x0, x1, 2", AluOp.ADD, 0, 1, 2)
    }
  }

  test("Decode SLLI") {
    compiled.doSim { dut =>
      checkIType(dut, "slli x0, x1, 2", AluOp.SLL, 0, 1, 2)
    }
  }

  test("Decode SLTI") {
    compiled.doSim { dut =>
      checkIType(dut, "slti x0, x1, 2", AluOp.LT, 0, 1, 2)
    }
  }

  test("Decode SLTIU") {
    compiled.doSim { dut =>
      checkIType(dut, "sltiu x0, x1, 2", AluOp.LTU, 0, 1, 2)
    }
  }

  test("Decode XORI") {
    compiled.doSim { dut =>
      checkIType(dut, "xori x0, x1, 2", AluOp.XOR, 0, 1, 2)
    }
  }

  test("Decode SRLI") {
    compiled.doSim { dut =>
      checkIType(dut, "srli x0, x1, 2", AluOp.SRL, 0, 1, 2)
    }
  }

  test("Decode SRAI") {
    compiled.doSim { dut =>
      // SRAI is distinguished from SRLI by a high bit in the immediate
      checkIType(dut, "srai x0, x1, 2", AluOp.SRA, 0, 1, 1026)
    }
  }

  test("Decode ORI") {
    compiled.doSim { dut =>
      checkIType(dut, "ori x0, x1, 2", AluOp.OR, 0, 1, 2)
    }
  }

  test("Decode ANDI") {
    compiled.doSim { dut =>
      checkIType(dut, "andi x0, x1, 2", AluOp.AND, 0, 1, 2)
    }
  }

  test("Decode LB") {
    compiled.doSim { dut =>
      checkIType(dut, "lb x0, 2(x1)", AluOp.ADD, 0, 1, 2, memRead = true, memMask = Some(MemType.Byte))
    }
  }

  test("Decode LH") {
    compiled.doSim { dut =>
      checkIType(dut, "lh x0, 2(x1)", AluOp.ADD, 0, 1, 2, memRead = true, memMask = Some(MemType.HalfWord))
    }
  }

  test("Decode LW") {
    compiled.doSim { dut =>
      checkIType(dut, "lw x0, 2(x1)", AluOp.ADD, 0, 1, 2, memRead = true, memMask = Some(MemType.Word))
    }
  }

  test("Decode LBU") {
    compiled.doSim { dut =>
      checkIType(dut, "lbu x0, 2(x1)", AluOp.ADD, 0, 1, 2, memRead = true, memMask = Some(MemType.ByteU))
    }
  }

  test("Decode LHU") {
    compiled.doSim { dut =>
      checkIType(dut, "lhu x0, 2(x1)", AluOp.ADD, 0, 1, 2, memRead = true, memMask = Some(MemType.HalfWordU))
    }
  }

  test("Decode JALR") {
    compiled.doSim { dut =>
      checkIType(dut, "jalr x0, x1, 2", AluOp.ADD, 0, 1, 2, branchType = BranchType.Jump)
    }
  }

  test("Decode JAL") {
    compiled.doSim { dut => checkJType(dut, "jal x0, 4", 0, 4) }
  }

  test("Decode BEQ") {
    compiled.doSim { dut => checkBType(dut, "beq x0, x1, 4", AluOp.EQ, 0, 1, 4) }
  }

  test("Decode BNE") {
    compiled.doSim { dut => checkBType(dut, "bne x0, x1, 4", AluOp.NE, 0, 1, 4) }
  }

  test("Decode BGE") {
    compiled.doSim { dut => checkBType(dut, "bge x0, x1, 4", AluOp.GE, 0, 1, 4) }
  }

  test("Decode BLT") {
    compiled.doSim { dut => checkBType(dut, "blt x0, x1, 4", AluOp.LT, 0, 1, 4) }
  }

  test("Decode BGEU") {
    compiled.doSim { dut => checkBType(dut, "bgeu x0, x1, 4", AluOp.GEU, 0, 1, 4) }
  }

  test("Decode BLTU") {
    compiled.doSim { dut => checkBType(dut, "bltu x0, x1, 4", AluOp.LTU, 0, 1, 4) }
  }

  test("Decode SB") {
    compiled.doSim { dut => checkSType(dut, "sb x0, 2(x1)", 1, 0, 2, MemType.Byte) }
  }

  test("Decode SH") {
    compiled.doSim { dut => checkSType(dut, "sh x0, 2(x1)", 1, 0, 2, MemType.HalfWord) }
  }

  test("Decode SW") {
    compiled.doSim { dut => checkSType(dut, "sw x0, 2(x1)", 1, 0, 2, MemType.Word) }
  }
}
