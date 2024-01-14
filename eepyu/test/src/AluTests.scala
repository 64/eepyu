package eepyu

import spinal.core.sim._

import org.scalatest.funsuite._

import scala.util.Random

class AluTests extends AnyFunSuite {
  val u32Max = (1L << 32) - 1

  // Scala and Java do not support unsigned arithmetic, so we have to hack it with Long... this is quite fiddily, so I've included some assertions to check we've done it right.
  def wrapLongAsU32(x: Long): Long = if (x.toInt.toLong < 0) {
    u32Max + x.toInt.toLong + 1
  } else {
    x & u32Max
  }

  assert(wrapLongAsU32(0) == 0)
  assert(wrapLongAsU32(1) == 1)
  assert(wrapLongAsU32(2) == 2)
  assert(wrapLongAsU32(-1) == u32Max)
  assert(wrapLongAsU32(-2) == u32Max - 1)
  assert(wrapLongAsU32(-3) == u32Max - 2)
  assert(wrapLongAsU32(-4) == u32Max - 3)
  assert(wrapLongAsU32(-2147483647) == 2147483649L)
  assert(wrapLongAsU32(-2147483648) == 2147483648L)
  assert(wrapLongAsU32(-2147483649L) == 2147483647)

  def checkAluOp(
      alu: Alu,
      op: AluOp.E,
      src1in: Long,
      src2in: Long
  ): Unit = {
    var src2 = src2in
    var src1 = src1in

    if (op == AluOp.SRL || op == AluOp.SLL || op == AluOp.SRA) {
      // Only the low 5 bits of src2 are used.
      src2 = src2 & ((1 << 5) - 1)
    }

    alu.io.en #= true
    alu.io.op #= op
    alu.io.src1 #= wrapLongAsU32(src1)
    alu.io.src2 #= wrapLongAsU32(src2)

    alu.clockDomain.waitSampling()
    alu.io.en #= false

    if (!alu.io.valid.toBoolean) {
      alu.clockDomain.waitSamplingWhere(alu.io.valid.toBoolean)
    }

    val expected = wrapLongAsU32(getModel(op)(src1, src2))
    val actual = alu.io.dst.toLong
    assert(
      expected == actual,
      f"AluOp.$op%s failed: operands were $src1%d ($src1%x) and $src2%d ($src2%x), expected $expected%x but got $actual%x"
    )
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
      val src1 = Random.nextLong((1L << 32) - 1)
      val src2 = Random.nextLong((1L << 32) - 1)
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
    case AluOp.SRL => (x, y) => (x & u32Max) >>> y
    case AluOp.SRA => (x, y) => x.toInt >> y
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

      checkAluOpRange(dut, AluOp.SLL, -20 until 20)
      checkRandom(dut, AluOp.SLL)
    }
  }

  test("AluOp.SRL") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(10 * 1000000)

      checkAluOpRange(dut, AluOp.SRL, -20 until 20)
      checkRandom(dut, AluOp.SRL)
    }
  }

  test("AluOp.SRA") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(10 * 1000000)

      checkAluOpRange(dut, AluOp.SRA, -20 until 20)
      checkRandom(dut, AluOp.SRA)
    }
  }
}
