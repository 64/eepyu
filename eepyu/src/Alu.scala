package eepyu

import spinal.core._

object AluOp extends SpinalEnum {
  val ADD, SUB, AND, OR, XOR, EQ, NE, SLL, SRL = newElement()
}

class Alu extends Component {
  val io = new Bundle {
    val op = in(AluOp)
    val src1 = in UInt (32 bits)
    val src2 = in UInt (32 bits)
    val en = in Bool ()

    val dst = out UInt (32 bits)
    val valid = out Bool ()
  }

  val shiftAmount = Reg(UInt(5 bits)) init 0
  val shiftVal = Reg(UInt(32 bits)) init 0
  val enabled = Reg(Bool()) init False

  io.valid := io.en

  when(io.en && !enabled && (io.op === AluOp.SLL || io.op === AluOp.SRL)) {
    shiftAmount := io.src2(0 until 5)
    shiftVal := io.src1
    enabled := True
    io.valid := False
  }

  when(enabled) {
    when(shiftAmount > 0) {
      io.valid := False
      shiftAmount := shiftAmount - 1

      when (io.op === AluOp.SLL) {
        shiftVal := shiftVal |<< 1
      }

      when (io.op === AluOp.SRL) {
        shiftVal := shiftVal |>> 1
      }
    }.otherwise {
      enabled := False
      io.valid := True
    }
  }

  io.dst := io.op.mux(
    AluOp.ADD -> (io.src1 + io.src2),
    AluOp.SUB -> (io.src1 - io.src2),
    AluOp.AND -> (io.src1 & io.src2),
    AluOp.OR -> (io.src1 | io.src2),
    AluOp.XOR -> (io.src1 ^ io.src2),
    AluOp.EQ -> (io.src1 === io.src2).asUInt.resized,
    AluOp.NE -> (io.src1 =/= io.src2).asUInt.resized,
    // Single cycle shifts, but big area usage
    // AluOp.SLL -> (io.src1 |<< io.src2(0 until 5)),
    // AluOp.SRL -> (io.src1 |>> io.src2(0 until 5)),
    AluOp.SLL -> shiftVal,
    AluOp.SRL -> shiftVal
  )
}

object AluVerilog extends App {
  Config.synth.generateVerilog(new Alu)
}
