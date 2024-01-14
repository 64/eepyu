package eepyu.util

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import eepyu.Alu
import eepyu.AluOp
import eepyu.Config

// Useful for area estimates, since it ensures the ALU doesn't get optimized away.

class AluUart extends Component {
  val io = new Bundle {
    val data = slave Flow (UInt(8 bits))
    val led = out Bool ()
  }

  // Usage:
  // val aluUart = new AluUart
  // aluUart.io.data << uartCtrl.io.read.asFlow.as(Flow(UInt(8 bits)))
  // io.redLeds(3) := aluUart.io.led

  val op = Reg(AluOp)
  val src1 = Reg(UInt(32 bits))
  val src2 = Reg(UInt(32 bits))
  val led = Reg(Bool()) init False
  val count = Counter(4) init 0

  io.led := led

  val alu = new Alu
  alu.io.op := op
  alu.io.src1 := src1
  alu.io.src2 := src2
  alu.io.en := False

  val input = io.data.map(x => x - '0'.toInt)

  val fsm = new StateMachine {
    val readOp = new State with EntryPoint
    val readSrc1 = new State
    val readSrc2 = new State
    val enableAlu = new State
    val commit = new State

    readOp.whenIsActive {
      when(io.data.fire) {
        op.assignFromBits(input.payload.asBits.resized)
        goto(readSrc1)
      }
    }

    readSrc1.whenIsActive {
      when(io.data.fire) {
        src1 := (src1 |<< 8) | input.payload.resize(32)
        count.increment()
        when(count.willOverflow) {
          goto(readSrc2)
        }
      }
    }

    readSrc2.whenIsActive {
      when(io.data.fire) {
        src2 := (src2 |<< 8) | input.payload.resize(32)
        count.increment()
        when(count.willOverflow) {
          goto(enableAlu)
        }
      }
    }

    enableAlu.whenIsActive {
      alu.io.en := True
      count.clear()
      // Hacky...
      when(alu.io.valid) {
        led := alu.io.dst >= 5
        goto(readOp)
      }.otherwise {
        goto(commit)
      }
    }

    commit.whenIsActive {
      when(alu.io.valid) {
        led := alu.io.dst >= 5
        goto(readOp)
      }
    }
  }
}
