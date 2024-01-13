package eepyu

import spinal.core._

object AluOp extends SpinalEnum {
  val ADD, SUB, AND, OR, XOR, EQ, NE = newElement()
}

class Alu extends Component {
  val io = new Bundle {
    val op = in(AluOp)
    val src1 = in UInt (32 bits)
    val src2 = in UInt (32 bits)

    val dst = out UInt (32 bits)
  }

  io.dst := io.op.mux(
    AluOp.ADD -> (io.src1 + io.src2),
    AluOp.SUB -> (io.src1 - io.src2),
    AluOp.AND -> (io.src1 & io.src2),
    AluOp.OR -> (io.src1 | io.src2),
    AluOp.XOR -> (io.src1 ^ io.src2),
    AluOp.EQ -> (io.src1 === io.src2).asUInt.resized,
    AluOp.NE -> (io.src1 =/= io.src2).asUInt.resized
  )
}

object AluVerilog extends App {
  Config.synth.generateVerilog(new Alu)
}
