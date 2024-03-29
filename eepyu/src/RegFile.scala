package eepyu

import spinal.core._
import spinal.core.formal._
import spinal.core.sim._

class RegFile extends Component {
  val io = new Bundle {
    val rs1 = in UInt (5 bits)
    val rs2 = in UInt (5 bits)
    val rs1Data = out UInt (32 bits)
    val rs2Data = out UInt (32 bits)

    val rd = in UInt (5 bits)
    val rdData = in UInt (32 bits)
    val writeValid = in Bool ()
  }

  // TODO: Formal
  // TODO: Don't store anything for the zero register

  val rs1mem = Mem(UInt(32 bits), Array.fill(32)(U(0))) simPublic
  val rs2mem = Mem(UInt(32 bits), Array.fill(32)(U(0))) simPublic

  io.rs1Data := rs1mem.readSync(io.rs1)
  io.rs2Data := rs2mem.readSync(io.rs2)

  when(io.rd =/= 0 && io.writeValid) {
    // report(Seq("Written register ", io.rd, " to value ", io.rdData))
    rs1mem.write(io.rd, io.rdData)
    rs2mem.write(io.rd, io.rdData)
  }
}

object RegFileVerilog extends App {
  Config.synth.generateVerilog(new RegFile)
}
