package eepyu

import spinal.core._

object AluOp extends SpinalEnum {
  val ADD, SUB, AND, OR, XOR, SLL, SRL, SRA, EQ, NE, LT, GE, LTU, GEU = newElement()
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
  val shifting = Reg(Bool()) init False

  val isShift = io.op === AluOp.SLL || io.op === AluOp.SRL || io.op === AluOp.SRA

  io.valid := io.en

  // when(io.en && !shifting && isShift) {
  //   // Begin shifting.
  //   shiftAmount := io.src2(0 until 5)
  //   shiftVal := io.src1
  //   shifting := True
  //
  //   // We could optimise this to set valid if the output is set this cycle (shiftAmount == 0).
  //   io.valid := False
  // }

  when(shifting) {
    when(shiftAmount > 0) {
      io.valid := False
      shiftAmount := shiftAmount - 1

      when(io.op === AluOp.SLL) {
        shiftVal := shiftVal |<< 1
      }
      when(io.op === AluOp.SRL) {
        shiftVal := shiftVal |>> 1
      }
      when(io.op === AluOp.SRA) {
        shiftVal := (shiftVal.asSInt |>> 1).asUInt
      }
    }.otherwise {
      shifting := False
      io.valid := True
    }
  }

  io.dst := io.op.mux(
    AluOp.ADD -> (io.src1 + io.src2),
    AluOp.SUB -> (io.src1 - io.src2),
    AluOp.AND -> (io.src1 & io.src2),
    AluOp.OR -> (io.src1 | io.src2),
    AluOp.XOR -> (io.src1 ^ io.src2),
    // Single cycle shifts, but big area usage
    AluOp.SLL -> (io.src1 |<< io.src2(0 until 5)),
    AluOp.SRL -> (io.src1 |>> io.src2(0 until 5)),
    AluOp.SRA -> (io.src1.asSInt |>> io.src2(0 until 5)).asUInt,
    // AluOp.SLL -> shiftVal,
    // AluOp.SRL -> shiftVal,
    // AluOp.SRA -> shiftVal,
    AluOp.EQ -> (io.src1 === io.src2).asUInt.resized,
    AluOp.NE -> (io.src1 =/= io.src2).asUInt.resized,
    AluOp.LT -> (io.src1.asSInt < io.src2.asSInt).asUInt.resized,
    AluOp.GE -> (io.src1.asSInt >= io.src2.asSInt).asUInt.resized,
    AluOp.LTU -> (io.src1 < io.src2).asUInt.resized,
    AluOp.GEU -> (io.src1 >= io.src2).asUInt.resized
  )
}

object AluVerilog extends App {
  Config.synth.generateVerilog(new Alu)
}
