package eepyu

import spinal.core._
import spinal.lib._

class MemoryPort(imemWidth: Int, memWidth: Int) extends Bundle with IMasterSlave {
  val imemReadAddr = UInt(imemWidth bits)
  val imemReadData = UInt(32 bits)

  val memAddr = UInt(memWidth bits)
  val memMask = Bits(4 bits)
  val memEnable = Bool()

  val memReadData = UInt(32 bits)
  val memWriteData = UInt(32 bits)
  val memWriteEnable = Bool()

  override def asMaster(): Unit = {
    in(imemReadData, memReadData)
    out(imemReadAddr, memAddr, memMask, memWriteData, memWriteEnable, memEnable)
  }
}

class Memory(imemWidth: Int, memWidth: Int, initialInstructionMemory: Seq[UInt] = Seq()) extends Component {
  val io = slave(new MemoryPort(imemWidth, memWidth))

  val padded = initialInstructionMemory.padTo(1 << imemWidth >> 2, U(0))

  val imem = new Mem(UInt(32 bits), 1 << imemWidth >> 2) init (padded)
  val mem = new Mem(UInt(32 bits), 1 << memWidth >> 2)

  io.imemReadData := imem.readSync(io.imemReadAddr >> 2)
  io.memReadData := mem.readWriteSync(
    address = io.memAddr >> 2,
    data = io.memWriteData, // TODO: WRONG !!!! needs to be shifted
    enable = io.memEnable,
    write = io.memWriteEnable,
    mask = io.memMask |<< io.memAddr(2 downto 0) // ignore misaligned. todo: shift in decoder
  )
}

object MemoryVerilog extends App {
  Config.synth.generateVerilog(new Memory(4, 8))
}
