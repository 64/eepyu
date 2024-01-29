package eepyu

import spinal.core._
import spinal.core.sim._

import org.scalatest.funsuite._

import scala.collection.mutable
import scala.util.Random

class TopTests extends AnyFunSuite {
  def uartTxByte(dut: Top, buffer: Int) = {
    val txd = dut.io.uart.rxd
    val rxd = dut.io.uart.txd
    val baudRate = 115200
    val baudPeriod = (dut.clockDomain.frequency.getValue.toDouble / baudRate.toDouble).toInt

    txd #= false
    dut.clockDomain.waitSampling(baudPeriod)

    for (i <- 0 to 7) {
      txd #= ((buffer >> i) & 1) != 0
      dut.clockDomain.waitSampling(baudPeriod)
    }

    txd #= true
    dut.clockDomain.waitSampling(baudPeriod)
  }

  // test("Core should toggle LED when RX byte 0") {
  //   Config.sim.compile(ClockedTop()).doSim { dut =>
  //     SimTimeout(10000)
  //     dut.clockDomain.forkStimulus(2)
  //     uartTxByte(dut, '1')
  //     dut.clockDomain.waitSampling(500)
  //     assert(!dut.io.redLeds(3).toBoolean)
  //     uartTxByte(dut, '0')
  //     dut.clockDomain.waitSamplingWhere(dut.io.redLeds(3).toBoolean)
  //   }
  // }

  test("Core should toggle LED") {
    Config.sim.compile(ClockedTop()).doSim { dut =>
      SimTimeout(10000)
      dut.clockDomain.forkStimulus(2)
      for (i <- 0 to 5) {
        dut.clockDomain.waitSamplingWhere(dut.io.redLeds(3).toBoolean)
        dut.clockDomain.waitSamplingWhere(!dut.io.redLeds(3).toBoolean)
      }
    }
  }

  // test("Core should error") {
  //   Config.sim.compile(ClockedTop()).doSim { dut =>
  //     SimTimeout(1000)
  //     dut.clockDomain.forkStimulus(2)
  //     dut.clockDomain.waitSamplingWhere(dut.io.redLeds(3).toBoolean)
  //   }
  // }

  // test("Core should not error") {
  //   Config.sim.compile(ClockedTop()).doSim { dut =>
  //     dut.clockDomain.forkStimulus(2)
  //     for (i <- 0 until 3000) {
  //       assert(!dut.io.redLeds(3).toBoolean)
  //       dut.clockDomain.waitSampling()
  //     }
  //   }
  // }

  // test("Core should error on UART TX") {
  //   Config.sim.compile(ClockedTop()).doSim { dut =>
  //     SimTimeout(10000)
  //     dut.clockDomain.forkStimulus(2)
  //     uartTxByte(dut, 1)
  //     uartTxByte(dut, 2)
  //     uartTxByte(dut, 3)
  //
  //     dut.clockDomain.waitSamplingWhere(dut.io.redLeds(3).toBoolean)
  //   }
  // }

  // test("UART TX/RX") {
  //   Config.sim.compile(ClockedTop()).doSim { dut =>
  //     val clock = dut.clockDomain
  //
  //     // Fork a process to generate the reset and the clock on the dut
  //     clock.forkStimulus(10)
  //     SimTimeout(10 * 1000000)
  //
  //     val txd = dut.io.uart.rxd
  //     val rxd = dut.io.uart.txd
  //     val baudRate = 115200
  //     val baudPeriod = (clock.frequency.getValue.toDouble / baudRate.toDouble).toInt
  //     println(s"Baud Period = ${baudPeriod} cycles")
  //
  //     def txByte(buffer: Int) = {
  //       txd #= false
  //       clock.waitSampling(baudPeriod)
  //
  //       for (i <- 0 to 7) {
  //         txd #= ((buffer >> i) & 1) != 0
  //         clock.waitSampling(baudPeriod)
  //       }
  //
  //       txd #= true
  //       clock.waitSampling(baudPeriod)
  //     }
  //
  //     val readQueue = mutable.Queue[Int]()
  //
  //     fork {
  //       sleep(1)
  //       waitUntil(rxd.toBoolean)
  //
  //       while (true) {
  //         waitUntil(!rxd.toBoolean)
  //         clock.waitSampling(baudPeriod / 2)
  //
  //         assert(!rxd.toBoolean)
  //         clock.waitSampling(baudPeriod)
  //
  //         var buffer = 0
  //         for (i <- 0 to 7) {
  //           if (rxd.toBoolean)
  //             buffer |= 1 << i
  //
  //           clock.waitSampling(baudPeriod)
  //         }
  //
  //         readQueue.enqueue(buffer)
  //       }
  //     }
  //
  //     def checkSort(list: List[Int]) = {
  //       println(s"Check sorting of list: ${list}")
  //
  //       for (i <- list) {
  //         txByte(i)
  //       }
  //
  //       // Wait a little bit for the sorting to take place
  //       clock.waitSampling(8 * 8 * 4)
  //
  //       for (i <- list.sorted) {
  //         clock.waitSampling(baudPeriod * (1 + 8 + 3))
  //         val data = readQueue.dequeue()
  //         // println(s"Dequeue'd ${data} (expected ${i})")
  //         assert(data == i)
  //       }
  //     }
  //
  //     checkSort((0 until 8).toList)
  //     checkSort((0 until 8).reverse.toList)
  //   }
  // }
}
