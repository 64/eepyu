package eepyu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

class CoreIO(imemWidth: Int, memWidth: Int) extends Bundle {
  val mem = master(new MemoryPort(imemWidth, memWidth))

  // TODO: Error?
  val error = out Bool ()

  val rvfi_valid = out Bool ()
  val rvfi_insn = out UInt (32 bits)
  val rvfi_order = out UInt (64 bits)
  val rvfi_halt = out Bool ()
}

class Core(val imemWidth: Int = 4, val memWidth: Int = 8) extends Component {
  val io = new CoreIO(imemWidth, memWidth)

  val regFile = new RegFile
  val pc = Reg(UInt(imemWidth bits)) init 0
  val fetchValid = Reg(Bool()) init False

  val DECODE = Payload(new DecoderIO())
  val ALU_RESULT = Payload(UInt(32 bits))
  val INST = Payload(Bits(32 bits))
  val PC = Payload(UInt(imemWidth bits))

  val fetch = Node()
  val decode = Node()
  val execute = Node()
  val writeback = Node()

  val f2d = StageLink(fetch, Node())
  // val sf2d = S2MLink(f2d.down, Node())
  val cf2d = CtrlLink(f2d.down, decode)

  val d2e = StageLink(decode, Node())
  // val sd2e = S2MLink(d2e.down, Node())
  val cd2e = CtrlLink(d2e.down, execute)

  val e2w = StageLink(execute, Node())
  // val se2w = S2MLink(e2w.down, Node())
  val ce2w = CtrlLink(e2w.down, writeback)

  val fetchArea = new fetch.Area {
    valid := fetchValid
    fetchValid := True // hack ?
    // io.mem.imemReadAddr := 0

    val nextPc = pc + 4

    when(isFiring) {
      pc := nextPc
    }

    io.mem.imemReadAddr := isFiring ? nextPc | pc
    PC := pc
    INST := io.mem.imemReadData.asBits
  }

  val decodeArea = new decode.Area {
    val decoder = new Decoder
    decoder.io.inst := INST
    DECODE := decoder.io

    regFile.io.rs1 := decoder.io.rs1
    regFile.io.rs2 := decoder.io.rs2

    val readsRs1 = decoder.io.rType || decoder.io.iType || decoder.io.sType || decoder.io.bType
    val readsRs2 = decoder.io.rType || decoder.io.sType || decoder.io.bType

    def hazardInStage(stage: Node) = {
      val nonZero = stage(DECODE).rd =/= 0
      val rs1Hazard = stage(DECODE).rd === decoder.io.rs1
      val rs2Hazard = stage(DECODE).rd === decoder.io.rs2
        stage.isValid && nonZero && ((readsRs1 && rs1Hazard) || (readsRs2 && rs2Hazard))
    }
    val rawHazardInExecute = hazardInStage(execute)
    val rawHazardInWriteback = hazardInStage(writeback)
    val anyHazard = rawHazardInExecute || rawHazardInWriteback

    when(anyHazard) {
      // report(Seq("e hazard: ", rawHazardInExecute, ", w hazard: ", rawHazardInWriteback))
      cf2d.haltIt()
    }
  }

  val executeArea = new execute.Area {
    val alu = new Alu
    alu.io.op := DECODE.aluOp
    alu.io.en := True
    ALU_RESULT := alu.io.dst
    ce2w.haltWhen(!alu.io.valid)

    alu.io.src1 := 0
    alu.io.src2 := 0

    when(DECODE.rType) {
      alu.io.src1 := regFile.io.rs1Data
      alu.io.src2 := regFile.io.rs2Data
    }

    when(DECODE.iType) {
      alu.io.src1 := regFile.io.rs1Data
      alu.io.src2 := DECODE.imm

      // todo: mem
    }

    when(DECODE.sType) {}

    when(DECODE.jType) {
      alu.io.src1 := regFile.io.rs1Data
      alu.io.src2 := PC + DECODE.imm

      when(isFiring) {
        // Set PC to branch target and flush pipe
        pc := alu.io.dst.resized
        cf2d.throwIt()
        fetchValid := False
        fetch.valid := False

        // Link register
        ALU_RESULT := (PC + 4).resized
      }
    }

    when(DECODE.bType) {
      alu.io.src1 := regFile.io.rs1Data
      alu.io.src2 := regFile.io.rs2Data

      when(isFiring && alu.io.dst(0)) {
        // hack
        pc := (PC + DECODE.imm).resized
        // report(Seq("branching to PC ", pc))
        cf2d.throwIt()
        fetchValid := False
        fetch.valid := False
      }
    }

    when(DECODE.uType) {
      when(DECODE.pcRel) {
        // todo: use ALU ?
        ALU_RESULT := (PC + DECODE.imm).resized
      }.otherwise {
        ALU_RESULT := DECODE.imm.resized
      }
    }

    io.mem.memWriteAddr := 0
    io.mem.memWriteData := 0
    io.mem.memWriteMask := B"1111"

    io.mem.memReadAddr := 0
  }

  val writebackArea = new writeback.Area {
    // TODO: can we do this during execute stage?
    regFile.io.rd := DECODE.rd
    regFile.io.rdData := ALU_RESULT
    regFile.io.writeValid := isFiring && (DECODE.rType || DECODE.iType || DECODE.jType || DECODE.uType)
    io.error := RegNextWhen(True, isFiring && DECODE.error) init False

    val order = Counter(64 bits)

    when(isFiring) {
      order.increment()
    }

    io.rvfi_valid := isFiring
    io.rvfi_insn := DECODE.inst.asUInt
    io.rvfi_order := order
    io.rvfi_halt := DECODE.error
  }

  Builder(f2d, /* sf2d, */ cf2d, d2e, /* sd2e, */ cd2e, e2w, /* se2w, */ ce2w)
}

object CoreVerilog extends App {
  Config.synth.generateVerilog(new Core)
}
