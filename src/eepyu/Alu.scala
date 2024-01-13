package eepyu

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.util.Random
import spinal.lib.fsm._
import eepyu.util.AluUart

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

object AluSim extends App {
  def checkAluOpRange(
      alu: Alu,
      op: AluOp.E,
      range: Range
  ) = {
    for (i <- range) {
      for (j <- range) {
        checkAluOp(alu, op, i, j)
      }
    }
  }

  def checkAluOp(
      alu: Alu,
      op: AluOp.E,
      src1: Long,
      src2: Long
  ) = {
    def reverse(x: Long): Long = scala.math.pow(2, 32).toLong - x
    def wrapNegatives(x: Long): Long = if (x < 0) { reverse(x.abs) }
    else { x }

    alu.io.op #= op
    alu.io.src1 #= wrapNegatives(src1)
    alu.io.src2 #= wrapNegatives(src2)

    sleep(1)

    val expected = wrapNegatives(getModel(op)(src1, src2))
    val actual = alu.io.dst.toLong
    assert(expected == actual, s"AluOp.$op failed: operands were $src1 and $src2, expected $expected but got $actual")
  }

  def addModel(x: Long, y: Long) = x + y
  def subModel(x: Long, y: Long) = x - y
  def andModel(x: Long, y: Long) = x & y
  def orModel(x: Long, y: Long) = x | y
  def xorModel(x: Long, y: Long) = x ^ y
  def eqModel(x: Long, y: Long) = if (x == y) 1 else 0
  def neModel(x: Long, y: Long) = if (x != y) 1 else 0

  def getModel(op: AluOp.E): (Long, Long) => Long = op match {
    case AluOp.ADD => addModel
    case AluOp.SUB => subModel
    case AluOp.AND => andModel
    case AluOp.OR  => orModel
    case AluOp.XOR => xorModel
    case AluOp.EQ  => eqModel
    case AluOp.NE  => neModel
  }

  def checkRandom(alu: Alu, op: AluOp.E) = {
    for (i <- 0 until 10000) {
      val src1 = Random.between(0, (2 << 32) - 1)
      val src2 = Random.between(0, (2 << 32) - 1)
      checkAluOp(alu, op, src1, src2)
    }
  }

  val compiled = Config.sim(args).compile(new Alu)

  compiled.doSim { dut =>
    checkAluOpRange(dut, AluOp.ADD, -20 until 20)
    checkRandom(dut, AluOp.ADD)
  }

  compiled.doSim { dut =>
    checkAluOpRange(dut, AluOp.SUB, -20 until 20)
    checkRandom(dut, AluOp.SUB)
  }

  compiled.doSim { dut =>
    checkAluOpRange(dut, AluOp.AND, 0 until 40)
    checkRandom(dut, AluOp.AND)
  }

  compiled.doSim { dut =>
    checkAluOpRange(dut, AluOp.OR, 0 until 40)
    checkRandom(dut, AluOp.OR)
  }

  compiled.doSim { dut =>
    checkAluOpRange(dut, AluOp.XOR, 0 until 40)
    checkRandom(dut, AluOp.XOR)
  }

  compiled.doSim { dut =>
    checkAluOpRange(dut, AluOp.EQ, 0 until 20)
    checkRandom(dut, AluOp.EQ)
  }

  compiled.doSim { dut =>
    checkAluOpRange(dut, AluOp.NE, 0 until 20)
    checkRandom(dut, AluOp.NE)
  }

  // compiled.doSim { dut =>
  //   checkAluOpRange(dut, AluOp.LT, -20 until 20)
  //   checkRandom(dut, AluOp.LT)
  // }
}

object AluUartSim extends App {
  Config.sim(args).compile(new AluUart).doSim { dut =>
    dut.io.data.valid #= false

    val clock = dut.clockDomain
    clock.forkStimulus(10)

    def txChars(dut: AluUart, chars: String) {
      clock.waitSampling()
      dut.io.data.valid #= true

      for (ch <- chars.toCharArray()) {
        dut.io.data.payload #= ch
        clock.waitSampling()
      }

      dut.io.data.valid #= false
      clock.waitSampling(2)
    }

    txChars(dut, "0")
    txChars(dut, "0000")
    txChars(dut, "0000")
    assert(!dut.io.led.toBoolean)

    txChars(dut, "0")
    txChars(dut, "0005")
    txChars(dut, "0000")
    assert(dut.io.led.toBoolean)

    txChars(dut, "0")
    txChars(dut, "0000")
    txChars(dut, "0005")
    assert(dut.io.led.toBoolean)

    txChars(dut, "0")
    txChars(dut, "0001")
    txChars(dut, "0001")
    assert(!dut.io.led.toBoolean)

    txChars(dut, "0")
    txChars(dut, "1000")
    txChars(dut, "1000")
    assert(dut.io.led.toBoolean)

    txChars(dut, "0")
    txChars(dut, "1110")
    txChars(dut, "0001")
    assert(dut.io.led.toBoolean)
  }
}

object AluVerilog extends App {
  Config.synth.generateVerilog(new Alu)
}
