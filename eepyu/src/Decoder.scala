package eepyu

import spinal.core._

object InstFormat extends Enumeration {
  val RType, IType, SType, JType, BType, UType = Value
}

object BranchType extends Enumeration {
  val None, Jump = Value
}

object Inst {
  def ADD = M"0000000----------000-----0110011"
  def SUB = M"0100000----------000-----0110011"
  def SLL = M"0000000----------001-----0110011"
  def SLT = M"0000000----------010-----0110011"
  def SLTU = M"0000000----------011-----0110011"
  def XOR = M"0000000----------100-----0110011"
  def SRL = M"0000000----------101-----0110011"
  def SRA = M"0100000----------101-----0110011"
  def OR = M"0000000----------110-----0110011"
  def AND = M"0000000----------111-----0110011"

  def ADDI = M"-----------------000-----0010011"
  def SLLI = M"0000000----------001-----0010011"
  def SLTI = M"-----------------010-----0010011"
  def SLTIU = M"-----------------011-----0010011"
  def XORI = M"-----------------100-----0010011"
  def SRLI = M"0000000----------101-----0010011"
  def SRAI = M"0100000----------101-----0010011"
  def ORI = M"-----------------110-----0010011"
  def ANDI = M"-----------------111-----0010011"

  def LB = M"-----------------000-----0000011"
  def LH = M"-----------------001-----0000011"
  def LW = M"-----------------010-----0000011"
  def LBU = M"-----------------100-----0000011"
  def LHU = M"-----------------101-----0000011"
  def SB = M"-----------------000-----0100011"
  def SH = M"-----------------001-----0100011"
  def SW = M"-----------------010-----0100011"

  def BEQ = M"-----------------000---0-1100011"
  def BNE = M"-----------------001---0-1100011"
  def BLT = M"-----------------100---0-1100011"
  def BGE = M"-----------------101---0-1100011"
  def BLTU = M"-----------------110---0-1100011"
  def BGEU = M"-----------------111---0-1100011"
  def JALR = M"-----------------000-----1100111"
  def JAL = M"----------0--------------1101111"
  def LUI = M"-------------------------0110111"
  def AUIPC = M"-------------------------0010111"

  val instMap = Map(
    // The branch types are a bit broken right now
    ADD -> (InstFormat.RType, AluOp.ADD, BranchType.None),
    SUB -> (InstFormat.RType, AluOp.SUB, BranchType.None),
    SLL -> (InstFormat.RType, AluOp.SLL, BranchType.None),
    SLT -> (InstFormat.RType, AluOp.LT, BranchType.None),
    SLTU -> (InstFormat.RType, AluOp.LTU, BranchType.None),
    XOR -> (InstFormat.RType, AluOp.XOR, BranchType.None),
    SRL -> (InstFormat.RType, AluOp.SRL, BranchType.None),
    SRA -> (InstFormat.RType, AluOp.SRA, BranchType.None),
    OR -> (InstFormat.RType, AluOp.OR, BranchType.None),
    AND -> (InstFormat.RType, AluOp.AND, BranchType.None),
    ADDI -> (InstFormat.IType, AluOp.ADD, BranchType.None),
    SLLI -> (InstFormat.IType, AluOp.SLL, BranchType.None),
    SLTI -> (InstFormat.IType, AluOp.LT, BranchType.None),
    SLTIU -> (InstFormat.IType, AluOp.LTU, BranchType.None),
    XORI -> (InstFormat.IType, AluOp.XOR, BranchType.None),
    SRLI -> (InstFormat.IType, AluOp.SRL, BranchType.None),
    SRAI -> (InstFormat.IType, AluOp.SRA, BranchType.None),
    ORI -> (InstFormat.IType, AluOp.OR, BranchType.None),
    ANDI -> (InstFormat.IType, AluOp.AND, BranchType.None),
    LB -> (InstFormat.IType, AluOp.ADD, BranchType.None),
    LH -> (InstFormat.IType, AluOp.ADD, BranchType.None),
    LW -> (InstFormat.IType, AluOp.ADD, BranchType.None),
    LBU -> (InstFormat.IType, AluOp.ADD, BranchType.None),
    LHU -> (InstFormat.IType, AluOp.ADD, BranchType.None),
    JALR -> (InstFormat.IType, AluOp.ADD, BranchType.Jump),
    SB -> (InstFormat.SType, AluOp.ADD, BranchType.None),
    SH -> (InstFormat.SType, AluOp.ADD, BranchType.None),
    SW -> (InstFormat.SType, AluOp.ADD, BranchType.None),
    BEQ -> (InstFormat.BType, AluOp.EQ, BranchType.None),
    BNE -> (InstFormat.BType, AluOp.NE, BranchType.None),
    BLT -> (InstFormat.BType, AluOp.LT, BranchType.None),
    BGE -> (InstFormat.BType, AluOp.GE, BranchType.None),
    BLTU -> (InstFormat.BType, AluOp.LTU, BranchType.None),
    BGEU -> (InstFormat.BType, AluOp.GEU, BranchType.None),
    JAL -> (InstFormat.JType, AluOp.ADD, BranchType.None),
    LUI -> (InstFormat.UType, AluOp.ADD, BranchType.None),
    AUIPC -> (InstFormat.UType, AluOp.ADD, BranchType.None)
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
  val pcRel = out Bool ()
  val branchType = out Bool ()

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

  io.pcRel := False
  io.branchType := False

  switch(io.inst) {
    import Inst._

    for ((inst, (format, aluOp, branchType)) <- Inst.instMap) {
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

        io.branchType := Bool(BranchType.Jump == branchType)
        io.aluOp := aluOp
        io.imm := imm.asUInt.resize(32 bits)
        formatSignal := True
      }
    }

    default {
      io.aluOp := AluOp.ADD
      io.error := True
      io.imm := 0 // ?
    }
  }
}

object DecoderVerilog extends App {
  Config.synth.generateVerilog(new Decoder)
}
