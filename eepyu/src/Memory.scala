package eepyu

import spinal.core._
import spinal.lib._

class MemoryPort(imemWidth: Int, memWidth: Int) extends Bundle with IMasterSlave {
  val imemReadAddr = UInt(imemWidth bits)
  val imemReadData = UInt(32 bits)

  val memAddr = UInt(memWidth bits)
  val memType = MemType()
  val memEnable = Bool()

  val memReadData = UInt(32 bits)
  val memWriteData = UInt(32 bits)
  val memWriteEnable = Bool()

  override def asMaster(): Unit = {
    in(imemReadData, memReadData)
    out(imemReadAddr, memAddr, memType, memWriteData, memWriteEnable, memEnable)
  }
}

class Memory(imemWidth: Int, memWidth: Int, initialInstructionMemory: Seq[UInt] = Seq()) extends Component {
  val io = slave(new MemoryPort(imemWidth, memWidth))

  val padded = initialInstructionMemory.padTo(1 << imemWidth >> 2, U(0))

  val imem = new Mem(UInt(32 bits), 1 << imemWidth >> 2) init (padded)
  val mem = new Mem(UInt(32 bits), 1 << memWidth >> 2)

  val mask = io.memType.mux(
    MemType.Byte -> B"0001",
    MemType.ByteU -> B"0001",
    MemType.HalfWord -> B"0011",
    MemType.HalfWordU -> B"0011",
    MemType.Word -> B"1111"
  )
  val maskShifted = mask |<< io.memAddr(2 downto 0) // ignore misaligned accesses. TODO: try shift in decoder

  val sext = io.memType.mux(
    MemType.Byte -> True,
    MemType.ByteU -> False,
    MemType.HalfWord -> True,
    MemType.HalfWordU -> False,
    MemType.Word -> False
  )

  io.imemReadData := imem.readSync(io.imemReadAddr >> 2)

  val readData = mem.readWriteSync(
    address = io.memAddr >> 2,
    data = io.memWriteData, // TODO: WRONG !!!! needs to be shifted
    enable = io.memEnable,
    write = io.memWriteEnable,
    mask = maskShifted
  )

  when(sext && !mask(1)) {
    // readDataExt := U(readData(7).asSInt.resize(24) ## readData(7 downto 0))
    io.memReadData := (
      (31 downto 7) -> readData(7),
      (7 downto 0) -> readData(7 downto 0)
    )
  }.elsewhen(sext && !mask(2)) {
    io.memReadData := (
      (31 downto 15) -> readData(15),
      (15 downto 0) -> readData(15 downto 0)
    )
  }.otherwise {
    io.memReadData := readData
  }
}

object MemoryVerilog extends App {
  Config.synth.generateVerilog(new Memory(4, 8))
}
