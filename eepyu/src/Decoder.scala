package eepyu

import spinal.core._

object InstFormat extends Enumeration {
  val RType, IType, SType, JType, BType, UType = Value
}

object BranchType extends Enumeration {
  val None, Jump = Value
}

object MemOp extends Enumeration {
  val None, Load, Store = Value
}

object MemType extends SpinalEnum {
  val Byte, ByteU, HalfWord, HalfWordU, Word = newElement()
  val X = Byte
}

object Inst {
  val ADD = M"0000000----------000-----0110011"
  val SUB = M"0100000----------000-----0110011"
  val SLL = M"0000000----------001-----0110011"
  val SLT = M"0000000----------010-----0110011"
  val SLTU = M"0000000----------011-----0110011"
  val XOR = M"0000000----------100-----0110011"
  val SRL = M"0000000----------101-----0110011"
  val SRA = M"0100000----------101-----0110011"
  val OR = M"0000000----------110-----0110011"
  val AND = M"0000000----------111-----0110011"

  val ADDI = M"-----------------000-----0010011"
  val SLLI = M"0000000----------001-----0010011"
  val SLTI = M"-----------------010-----0010011"
  val SLTIU = M"-----------------011-----0010011"
  val XORI = M"-----------------100-----0010011"
  val SRLI = M"0000000----------101-----0010011"
  val SRAI = M"0100000----------101-----0010011"
  val ORI = M"-----------------110-----0010011"
  val ANDI = M"-----------------111-----0010011"

  val LB = M"-----------------000-----0000011"
  val LH = M"-----------------001-----0000011"
  val LW = M"-----------------010-----0000011"
  val LBU = M"-----------------100-----0000011"
  val LHU = M"-----------------101-----0000011"
  val SB = M"-----------------000-----0100011"
  val SH = M"-----------------001-----0100011"
  val SW = M"-----------------010-----0100011"

  val BEQ = M"-----------------000---0-1100011"
  val BNE = M"-----------------001---0-1100011"
  val BLT = M"-----------------100---0-1100011"
  val BGE = M"-----------------101---0-1100011"
  val BLTU = M"-----------------110---0-1100011"
  val BGEU = M"-----------------111---0-1100011"
  val JALR = M"-----------------000-----1100111"
  val JAL = M"----------0--------------1101111"
  val LUI = M"-------------------------0110111"
  val AUIPC = M"-------------------------0010111"

  val instMap = Map(
    // The branch types are a bit broken right now
    ADD -> (InstFormat.RType, AluOp.ADD, MemOp.None, MemType.X, BranchType.None),
    SUB -> (InstFormat.RType, AluOp.SUB, MemOp.None, MemType.X, BranchType.None),
    SLL -> (InstFormat.RType, AluOp.SLL, MemOp.None, MemType.X, BranchType.None),
    SLT -> (InstFormat.RType, AluOp.LT, MemOp.None, MemType.X, BranchType.None),
    SLTU -> (InstFormat.RType, AluOp.LTU, MemOp.None, MemType.X, BranchType.None),
    XOR -> (InstFormat.RType, AluOp.XOR, MemOp.None, MemType.X, BranchType.None),
    SRL -> (InstFormat.RType, AluOp.SRL, MemOp.None, MemType.X, BranchType.None),
    SRA -> (InstFormat.RType, AluOp.SRA, MemOp.None, MemType.X, BranchType.None),
    OR -> (InstFormat.RType, AluOp.OR, MemOp.None, MemType.X, BranchType.None),
    AND -> (InstFormat.RType, AluOp.AND, MemOp.None, MemType.X, BranchType.None),
    ADDI -> (InstFormat.IType, AluOp.ADD, MemOp.None, MemType.X, BranchType.None),
    SLLI -> (InstFormat.IType, AluOp.SLL, MemOp.None, MemType.X, BranchType.None),
    SLTI -> (InstFormat.IType, AluOp.LT, MemOp.None, MemType.X, BranchType.None),
    SLTIU -> (InstFormat.IType, AluOp.LTU, MemOp.None, MemType.X, BranchType.None),
    XORI -> (InstFormat.IType, AluOp.XOR, MemOp.None, MemType.X, BranchType.None),
    SRLI -> (InstFormat.IType, AluOp.SRL, MemOp.None, MemType.X, BranchType.None),
    SRAI -> (InstFormat.IType, AluOp.SRA, MemOp.None, MemType.X, BranchType.None),
    ORI -> (InstFormat.IType, AluOp.OR, MemOp.None, MemType.X, BranchType.None),
    ANDI -> (InstFormat.IType, AluOp.AND, MemOp.None, MemType.X, BranchType.None),
    LB -> (InstFormat.IType, AluOp.ADD, MemOp.Load, MemType.Byte, BranchType.None),
    LH -> (InstFormat.IType, AluOp.ADD, MemOp.Load, MemType.HalfWord, BranchType.None),
    LW -> (InstFormat.IType, AluOp.ADD, MemOp.Load, MemType.Word, BranchType.None),
    LBU -> (InstFormat.IType, AluOp.ADD, MemOp.Load, MemType.ByteU, BranchType.None),
    LHU -> (InstFormat.IType, AluOp.ADD, MemOp.Load, MemType.HalfWordU, BranchType.None),
    JALR -> (InstFormat.IType, AluOp.ADD, MemOp.None, MemType.X, BranchType.Jump),
    SB -> (InstFormat.SType, AluOp.ADD, MemOp.Store, MemType.Byte, BranchType.None),
    SH -> (InstFormat.SType, AluOp.ADD, MemOp.Store, MemType.HalfWord, BranchType.None),
    SW -> (InstFormat.SType, AluOp.ADD, MemOp.Store, MemType.Word, BranchType.None),
    BEQ -> (InstFormat.BType, AluOp.EQ, MemOp.None, MemType.X, BranchType.None),
    BNE -> (InstFormat.BType, AluOp.NE, MemOp.None, MemType.X, BranchType.None),
    BLT -> (InstFormat.BType, AluOp.LT, MemOp.None, MemType.X, BranchType.None),
    BGE -> (InstFormat.BType, AluOp.GE, MemOp.None, MemType.X, BranchType.None),
    BLTU -> (InstFormat.BType, AluOp.LTU, MemOp.None, MemType.X, BranchType.None),
    BGEU -> (InstFormat.BType, AluOp.GEU, MemOp.None, MemType.X, BranchType.None),
    JAL -> (InstFormat.JType, AluOp.ADD, MemOp.None, MemType.X, BranchType.None),
    LUI -> (InstFormat.UType, AluOp.ADD, MemOp.None, MemType.X, BranchType.None),
    AUIPC -> (InstFormat.UType, AluOp.ADD, MemOp.None, MemType.X, BranchType.None)
  )
}

case class Imm(inst: Bits) extends Area {
  // immediates
  def i = inst(31 downto 20)
  def h = inst(31 downto 24)
  def s = inst(31 downto 25) ## inst(11 downto 7)
  def b = inst(31) ## inst(7) ## inst(30 downto 25) ## inst(11 downto 8)
  def u = inst(31 downto 12) ## U"x000"
  def j = inst(31) ## inst(19 downto 12) ## inst(20) ## inst(30 downto 21)
  def z = inst(19 downto 15)

  // sign-extend immediates
  def i_sext = B((19 downto 0) -> i(11)) ## i
  def h_sext = B((23 downto 0) -> h(7)) ## h
  def s_sext = B((19 downto 0) -> s(11)) ## s
  def b_sext = B((18 downto 0) -> b(11)) ## b ## False
  def j_sext = B((10 downto 0) -> j(19)) ## j ## False
}

class DecoderIO extends Bundle {
  val inst = in Bits (32 bits)

  val rType = out Bool ()
  val iType = out Bool ()
  val sType = out Bool ()
  val jType = out Bool ()
  val bType = out Bool ()
  val uType = out Bool ()

  // hacks
  val pcRel = out Bool ()
  val branchType = out Bool ()

  val memOp = out Bool ()
  val memWriteEnable = out Bool ()
  val memMask = out(MemType())

  val rs1 = out UInt (5 bits)
  val rs2 = out UInt (5 bits)
  val rd = out UInt (5 bits)

  val imm = out UInt (32 bits)

  val aluOp = out(AluOp())

  val error = out Bool ()
}

class Decoder extends Component {
  val io = new DecoderIO

  io.rs1 := io.inst(19 downto 15).asUInt
  io.rs2 := io.inst(24 downto 20).asUInt
  io.rd := io.inst(11 downto 7).asUInt

  io.error := False
  io.rType := False
  io.iType := False
  io.sType := False
  io.jType := False
  io.bType := False
  io.uType := False

  io.memOp := False
  io.memWriteEnable.assignDontCare()
  io.memMask.assignDontCare()

  io.imm.assignDontCare()
  io.aluOp.assignDontCare()

  io.pcRel := False
  io.branchType := False

  switch(io.inst) {
    import Inst._

    // TODO: Use assignDontCare where possible

    for ((inst, (format, aluOp, memOp, memMask, branchType)) <- Inst.instMap) {
      is(inst) {
        val (formatSignal, imm) = format match {
          case InstFormat.RType => (io.rType, B(0))
          case InstFormat.IType => (io.iType, Imm(io.inst).i_sext)
          case InstFormat.SType => (io.sType, Imm(io.inst).s_sext)
          case InstFormat.JType => (io.jType, Imm(io.inst).j_sext)
          case InstFormat.BType => (io.bType, Imm(io.inst).b_sext)
          case InstFormat.UType => (io.uType, Imm(io.inst).u)
        }

        // bit ad-hoc
        if (inst == AUIPC) {
          io.pcRel := True
        }

        if (memOp == MemOp.Load) {
          io.memOp := True
          io.memWriteEnable := False
          io.memMask := memMask
        } else if (memOp == MemOp.Store) {
          io.memOp := True
          io.memWriteEnable := True
          io.memMask := memMask
        } else {
          io.memOp := False
        }

        io.branchType := Bool(BranchType.Jump == branchType)

        io.aluOp := aluOp
        io.imm := imm.asUInt.resize(32 bits)
        formatSignal := True
      }
    }

    default {
      io.error := True
    }
  }
}

object DecoderVerilog extends App {
  Config.synth.generateVerilog(new Decoder)
}
