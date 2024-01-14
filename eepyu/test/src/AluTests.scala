package eepyu

import spinal.core.sim._

import org.scalatest.funsuite._

import scala.util.Random

class AluTests extends AnyFunSuite {
  def checkAluOp(
      alu: Alu,
      op: AluOp.E,
      src1: Long,
      src2: Long
  ) = {
    def reverse(x: Long): Long = scala.math.pow(2, 32).toLong - x
    def wrapNegatives(x: Long): Long = if (x < 0) { reverse(x.abs) }
    else { x }

    alu.io.en #= true
    alu.io.op #= op
    alu.io.src1 #= wrapNegatives(src1)
    alu.io.src2 #= wrapNegatives(src2)

    alu.clockDomain.waitSampling()
    alu.io.en #= false

    if (!alu.io.valid.toBoolean) {
      alu.clockDomain.waitSamplingWhere(alu.io.valid.toBoolean)
    }

    val expected = wrapNegatives(getModel(op)(src1, src2))
    val actual = alu.io.dst.toLong
    assert(expected == actual, s"AluOp.$op failed: operands were $src1 and $src2, expected $expected but got $actual")
  }

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

  def checkRandom(alu: Alu, op: AluOp.E) = {
    for (i <- 0 until 1000) {
      val src1 = Random.between(0, (2 << 32) - 1)
      val src2 = Random.between(0, (2 << 32) - 1)
      checkAluOp(alu, op, src1, src2)
    }
  }

  def getModel(op: AluOp.E): (Long, Long) => Long = op match {
    case AluOp.ADD => (x, y) => x + y
    case AluOp.SUB => (x, y) => x - y
    case AluOp.AND => (x, y) => x & y
    case AluOp.OR  => (x, y) => x | y
    case AluOp.XOR => (x, y) => x ^ y
    case AluOp.EQ  => (x, y) => if (x == y) 1 else 0
    case AluOp.NE  => (x, y) => if (x != y) 1 else 0
    case AluOp.SLL => (x, y) => x << y
    case AluOp.SRL => (x, y) => x >> y
  }

  val compiled = Config.sim.compile(new Alu)

  test("AluOp.ADD") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      checkAluOpRange(dut, AluOp.ADD, -20 until 20)
      checkRandom(dut, AluOp.ADD)
    }
  }

  test("AluOp.SUB") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      checkAluOpRange(dut, AluOp.SUB, -20 until 20)
      checkRandom(dut, AluOp.SUB)
    }
  }

  test("AluOp.AND") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      checkAluOpRange(dut, AluOp.AND, 0 until 40)
      checkRandom(dut, AluOp.AND)
    }
  }

  test("AluOp.OR") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      checkAluOpRange(dut, AluOp.OR, 0 until 40)
      checkRandom(dut, AluOp.OR)
    }
  }

  test("AluOp.XOR") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      checkAluOpRange(dut, AluOp.XOR, 0 until 40)
      checkRandom(dut, AluOp.XOR)
    }
  }

  test("AluOp.EQ") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      checkAluOpRange(dut, AluOp.EQ, 0 until 20)
      checkRandom(dut, AluOp.EQ)
    }
  }

  test("AluOp.NE") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      checkAluOpRange(dut, AluOp.NE, 0 until 20)
      checkRandom(dut, AluOp.NE)
    }
  }

  test("AluOp.SLL") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(10 * 1000000)

      checkAluOpRange(dut, AluOp.SLL, 0 until 20)
      checkRandom(dut, AluOp.SLL)
    }
  }

  test("AluOp.SRL") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(10 * 1000000)

      checkAluOpRange(dut, AluOp.SRL, 0 until 20)
      checkRandom(dut, AluOp.SRL)
    }
  }
}
