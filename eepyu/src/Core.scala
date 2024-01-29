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
  val rvfi_pc = out UInt (imemWidth bits)
  val rvfi_halt = out Bool ()
}

class Core(val imemWidth: Int = 5, val memWidth: Int = 3) extends Component {
  val io = new CoreIO(imemWidth, memWidth)

  val regFile = new RegFile

  val DECODE = Payload(new DecoderIO())
  val WRITE_BACK_VALUE = Payload(UInt(32 bits))
  val INST = Payload(Bits(32 bits))
  val PC = Payload(UInt(imemWidth bits))

  val flush = False

  val fetch, decode, execute, writeback = CtrlLink()
  val f2d = StageLink(fetch.down, decode.up)
  val d2e = StageLink(decode.down, execute.up)
  val e2w = StageLink(execute.down, writeback.up)

  val fetchArea = new fetch.Area {
    up.valid := True

    val pc = Reg(UInt(imemWidth bits)) init 0

    // Hack. The problem is that the PC stage doesn't hold the imemReadAddr when the fetch / decode stage has stalled.
    val oldPc = RegNextWhen(pc, isValid && isReady) init 0
    val newPc = pc

    when(isValid && isReady) {
      io.mem.imemReadAddr := newPc
      pc := pc + 4
    }.otherwise {
      io.mem.imemReadAddr := oldPc
    }

    PC := newPc
  }

  val decodeArea = new decode.Area {
    val decoder = new Decoder
    decoder.io.inst := io.mem.imemReadData.asBits
    DECODE := decoder.io

    regFile.io.rs1 := decoder.io.rs1
    regFile.io.rs2 := decoder.io.rs2

    val readsRs1 = decoder.io.rType || decoder.io.iType || decoder.io.sType || decoder.io.bType
    val readsRs2 = decoder.io.rType || decoder.io.sType || decoder.io.bType

    def hazardInStage(stage: CtrlLink) = {
      val nonZero = stage(DECODE).rd =/= 0
      val rs1Hazard = stage(DECODE).rd === decoder.io.rs1
      val rs2Hazard = stage(DECODE).rd === decoder.io.rs2
      stage.isValid && nonZero && ((readsRs1 && rs1Hazard) || (readsRs2 && rs2Hazard))
    }
    val rawHazardInExecute = hazardInStage(execute)
    val rawHazardInWriteback = hazardInStage(writeback)
    val anyHazard = rawHazardInExecute || rawHazardInWriteback

    // The Spinal Pipeline API seems to get confused if we haltIt() on the same cycle that a flush is happening (because we simultaneously throw and halt).
    when(anyHazard && !flush) {
      // report(Seq("e hazard: ", rawHazardInExecute, ", w hazard: ", rawHazardInWriteback))
      haltIt()
    }
  }

  val executeArea = new execute.Area {
    val alu = new Alu

    // Pulse ALU enable high for one cycle
    alu.io.en := isValid && (RegNextWhen(alu.io.valid, isValid) init True)
    alu.io.src1 := regFile.io.rs1Data
    alu.io.src2 := regFile.io.rs2Data
    alu.io.op := DECODE.aluOp
    execute.haltWhen(!alu.io.valid)

    io.mem.memWriteEnable := isValid && DECODE.memWriteEnable
    io.mem.memEnable := isValid && DECODE.memOp && !DECODE.error && !io.error
    io.mem.memType := DECODE.memMask
    io.mem.memAddr := alu.io.dst.resized
    io.mem.memWriteData := regFile.io.rs2Data

    val pcPlusImm = PC + DECODE.imm
    val pcPlusFour = PC + 4

    for (stage <- List(fetch, decode)) {
      stage.throwWhen(flush, usingReady = true)
    }

    WRITE_BACK_VALUE.assignDontCare()

    when(isValid && DECODE.rType) {
      WRITE_BACK_VALUE := alu.io.dst
    }

    when(isValid && DECODE.iType) {
      alu.io.src2 := DECODE.imm
      WRITE_BACK_VALUE := alu.io.dst // ignored for loads

      when(DECODE.branchType) {
        fetchArea.pc := alu.io.dst.resized
        flush := True

        WRITE_BACK_VALUE := pcPlusFour.resized
      }
    }

    when(isValid && DECODE.sType) {
      alu.io.src2 := DECODE.imm
      io.mem.memAddr := alu.io.dst.resized
    }

    when(isValid && DECODE.jType) {
      alu.io.src2 := pcPlusImm

      // Set PC to branch target and flush pipe
      fetchArea.pc := alu.io.dst.resized
      flush := True

      // Link register
      WRITE_BACK_VALUE := pcPlusFour.resized
      // report(Seq("jumping to PC ", alu.io.dst.resized))
    }
    when(isValid && DECODE.bType) {
      when(alu.io.dst(0)) {
        // hack
        val newPc = pcPlusImm.resized
        fetchArea.pc := newPc
        flush := True
        // report(Seq("branching to PC ", newPc))
      }
    }
    when(isValid && DECODE.uType) {
      when(DECODE.pcRel) {
        // todo: use ALU ?
        WRITE_BACK_VALUE := pcPlusImm
      }.otherwise {
        WRITE_BACK_VALUE := DECODE.imm.resized
      }
    }
  }

  val writebackArea = new writeback.Area {
    // TODO: use decoder bit
    when(DECODE.memOp) {
      regFile.io.rdData := io.mem.memReadData
    }.otherwise {
      regFile.io.rdData := WRITE_BACK_VALUE
    }

    val writebackValid = isValid && !DECODE.error && !io.error
    val writebackErrorThisCycle = isValid && DECODE.error

    regFile.io.rd := DECODE.rd
    regFile.io.writeValid := writebackValid && !(DECODE.bType || DECODE.sType)
    io.error := RegNextWhen(True, writebackErrorThisCycle) init False

    val order = Counter(64 bits)

    when(isValid) {
      order.increment()
    }

    io.rvfi_valid := isValid && !io.error
    io.rvfi_insn := DECODE.inst.asUInt
    io.rvfi_order := order
    io.rvfi_halt := io.error || writebackErrorThisCycle
    io.rvfi_pc := PC
  }

  Builder(fetch, f2d, decode, d2e, execute, e2w, writeback)
}

object CoreVerilog extends App {
  Config.synth.generateVerilog(new Core)
}
