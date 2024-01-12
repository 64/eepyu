package ice

import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import scala.util.Random

object TopSim extends App {
  Config.sim(args).compile(ClockedTop()).doSim { dut =>
    val clock = dut.clockDomain

    // Fork a process to generate the reset and the clock on the dut
    clock.forkStimulus(10)
    SimTimeout(10 * 1000000)

    val txd = dut.io.uart.rxd
    val rxd = dut.io.uart.txd
    val baudRate = 115200
    val baudPeriod = (clock.frequency.getValue.toDouble / baudRate.toDouble).toInt
    println(s"Baud Period = ${baudPeriod} cycles")

    def txByte(buffer: Int) = {
      txd #= false
      clock.waitSampling(baudPeriod)

      for (i <- 0 to 7) {
        txd #= ((buffer >> i) & 1) != 0
        clock.waitSampling(baudPeriod)
      }

      txd #= true
      clock.waitSampling(baudPeriod)
    }

    val readQueue = mutable.Queue[Int]()

    fork {
      sleep(1)
      waitUntil(rxd.toBoolean)

      while (true) {
        waitUntil(!rxd.toBoolean)
        clock.waitSampling(baudPeriod / 2)

        assert(!rxd.toBoolean)
        clock.waitSampling(baudPeriod)

        var buffer = 0
        for (i <- 0 to 7) {
          if (rxd.toBoolean)
            buffer |= 1 << i

          clock.waitSampling(baudPeriod)
        }

        readQueue.enqueue(buffer)
      }
    }


    def checkSort(list: List[Int]) = {
      println(s"Check sorting of list: ${list}")

      for (i <- list) {
        txByte(i)
      }

      // Wait a little bit for the sorting to take place
      clock.waitSampling(8 * 8 * 4)

      for (i <- list.sorted) {
        clock.waitSampling(baudPeriod * (1 + 8 + 3))
        val data = readQueue.dequeue()
        // println(s"Dequeue'd ${data} (expected ${i})")
        assert(data == i)
      }
    }

    checkSort((0 until 8).toList)
    checkSort((0 until 8).reverse.toList)

    for (i <- 0 until 50) {
      val list = (0 until 8).map(x => Random.between(0, 256)).toList
      checkSort(list)
    }
  }
}
