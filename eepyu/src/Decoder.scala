package eepyu

import spinal.core._

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

  val rTypeInstructions = List(ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND)
  val iTypeInstructions = List(ADDI, SLLI, SLTI, SLTIU, XORI, SRLI, SRAI, ORI, ANDI, LB, LH, LW, LBU, LHU, JALR)
  val sTypeInstructions = List(SB, SH, SW)
  val jTypeInstructions = List(JAL)
  val bTypeInstructions = List(BEQ, BNE, BLT, BGE, BLTU, BGEU)

  val allInstructions =
    rTypeInstructions ++ iTypeInstructions ++ sTypeInstructions ++ jTypeInstructions ++ bTypeInstructions
}

class Decoder extends Component {
  val io = new Bundle {
    val inst = in UInt (32 bits)

    val rType = out Bool ()
    val iType = out Bool ()
    val sType = out Bool ()
    val jType = out Bool ()
    val bType = out Bool ()

    val error = out Bool ()
  }

  import Inst._

  io.error := False
  io.rType := False
  io.iType := False
  io.sType := False
  io.jType := False
  io.bType := False

  switch(io.inst) {
    for (inst <- Inst.rTypeInstructions) {
      is(inst) {
        io.rType := True
      }
    }
    for (inst <- Inst.iTypeInstructions) {
      is(inst) {
        io.iType := True
      }
    }
    for (inst <- Inst.sTypeInstructions) {
      is(inst) {
        io.sType := True
      }
    }
    for (inst <- Inst.jTypeInstructions) {
      is(inst) {
        io.jType := True
      }
    }
    for (inst <- Inst.bTypeInstructions) {
      is(inst) {
        io.bType := True
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
