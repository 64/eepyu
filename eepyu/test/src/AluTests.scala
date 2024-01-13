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

    alu.io.op #= op
    alu.io.src1 #= wrapNegatives(src1)
    alu.io.src2 #= wrapNegatives(src2)

    sleep(1)

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
    for (i <- 0 until 10000) {
      val src1 = Random.between(0, (2 << 32) - 1)
      val src2 = Random.between(0, (2 << 32) - 1)
      checkAluOp(alu, op, src1, src2)
    }
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

  val compiled = Config.sim.compile(new Alu)

  test("AluOp.ADD") {
    compiled.doSim { dut =>
      checkAluOpRange(dut, AluOp.ADD, -20 until 20)
      checkRandom(dut, AluOp.ADD)
    }
  }

  test("AluOp.SUB") {
    compiled.doSim { dut =>
      checkAluOpRange(dut, AluOp.SUB, -20 until 20)
      checkRandom(dut, AluOp.SUB)
    }
  }

  test("AluOp.AND") {
    compiled.doSim { dut =>
      checkAluOpRange(dut, AluOp.AND, 0 until 40)
      checkRandom(dut, AluOp.AND)
    }
  }

  test("AluOp.OR") {
    compiled.doSim { dut =>
      checkAluOpRange(dut, AluOp.OR, 0 until 40)
      checkRandom(dut, AluOp.OR)
    }
  }

  test("AluOp.XOR") {
    compiled.doSim { dut =>
      checkAluOpRange(dut, AluOp.XOR, 0 until 40)
      checkRandom(dut, AluOp.XOR)
    }
  }

  test("AluOp.EQ") {
    compiled.doSim { dut =>
      checkAluOpRange(dut, AluOp.EQ, 0 until 20)
      checkRandom(dut, AluOp.EQ)
    }
  }

  test("AluOp.NE") {
    compiled.doSim { dut =>
      checkAluOpRange(dut, AluOp.NE, 0 until 20)
      checkRandom(dut, AluOp.NE)
    }
  }
}
