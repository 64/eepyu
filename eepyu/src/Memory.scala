package eepyu

import spinal.core._
import spinal.lib._

class MemoryPort(imemWidth: Int, memWidth: Int) extends Bundle with IMasterSlave {
  val imemReadAddr = UInt(imemWidth bits)
  val imemReadData = UInt(32 bits)

  val memReadAddr = UInt(memWidth bits)
  val memReadData = UInt(32 bits)

  val memWriteAddr = UInt(memWidth bits)
  val memWriteData = UInt(32 bits)
  val memWriteMask = Bits(4 bits)

  override def asMaster(): Unit = {
    in(imemReadData, memReadData)
    out(imemReadAddr, memReadAddr, memWriteAddr, memWriteData, memWriteMask)
  }
}

class Memory(imemWidth: Int, memWidth: Int, initialInstructionMemory: Seq[UInt] = Seq()) extends Component {
  val io = slave(new MemoryPort(imemWidth, memWidth))

  val padded = initialInstructionMemory.padTo(1 << imemWidth >> 2, U(0))

  val imem = new Mem(UInt(32 bits), 1 << imemWidth >> 2) init (padded)
  val mem = new Mem(UInt(32 bits), 1 << memWidth >> 2)

  io.imemReadData := imem.readSync(io.imemReadAddr >> 2)
  io.memReadData := mem.readSync(io.memReadAddr >> 2)

  val mask = Bits(4 bits)
  mask := (
    (31 downto 24) -> io.memWriteMask(3),
    (23 downto 16) -> io.memWriteMask(2),
    (15 downto 8) -> io.memWriteMask(1),
    (7 downto 0) -> io.memWriteMask(0)
  )

  mem.write(io.memWriteAddr >> 2, data = io.memWriteData, mask = mask)
}

object MemoryVerilog extends App {
  Config.synth.generateVerilog(new Memory(4, 8))
}
