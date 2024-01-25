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

class Core(val imemWidth: Int = 4, val memWidth: Int = 8) extends Component {
  val io = new CoreIO(imemWidth, memWidth)

  val regFile = new RegFile
  val fetchValid = Reg(Bool()) init False

  val DECODE = Payload(new DecoderIO())
  val WRITE_BACK_VALUE = Payload(UInt(32 bits))
  val INST = Payload(Bits(32 bits))
  val PC = Payload(UInt(imemWidth bits))

  val genPc = CtrlLink()

  val fetch, decode, execute, writeback = CtrlLink()
  val pc2f = StageLink(genPc.down, fetch.up)
  val f2d = StageLink(fetch.down, decode.up)
  val d2e = StageLink(decode.down, execute.up)
  val e2w = StageLink(execute.down, writeback.up)

  // val fetch = Node()
  // val decode = Node()
  // val execute = Node()
  // val writeback = Node()
  //
  // val f2d = StageLink(fetch, Node())
  // // val sf2d = S2MLink(f2d.down, Node())
  // val cf2d = CtrlLink(f2d.down, decode)
  //
  // val d2e = StageLink(decode, Node())
  // // val sd2e = S2MLink(d2e.down, Node())
  // val cd2e = CtrlLink(d2e.down, execute)
  //
  // val e2w = StageLink(execute, Node())
  // // val se2w = S2MLink(e2w.down, Node())
  // val ce2w = CtrlLink(e2w.down, writeback)

  val pcArea = new genPc.Area {
    val pc = Reg(UInt(imemWidth bits)) init 0

    when(isValid && isReady) {
      pc := pc + 4
    }

    up.valid := True
    PC := pc
  }

  val fetchArea = new fetch.Area {
    val oldPc = RegNextWhen(up(PC), isValid && isReady) init 0
    val newPc = up(PC)

    // Hack. The problem is that the PC stage doesn't hold the imemReadAddr when the fetch / decode stage has stalled.
    io.mem.imemReadAddr := (isValid && isReady) ? newPc | oldPc
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

    when(anyHazard) {
      // report(Seq("e hazard: ", rawHazardInExecute, ", w hazard: ", rawHazardInWriteback))
      haltIt()
    }
  }

  val executeArea = new execute.Area {
    val alu = new Alu
    alu.io.op := DECODE.aluOp
    alu.io.en := True
    WRITE_BACK_VALUE.assignDontCare()
    execute.haltWhen(!alu.io.valid)

    alu.io.src1.assignDontCare()
    alu.io.src2.assignDontCare()

    // io.mem.memAddr.assignDontCare()
    // io.mem.memWriteData.assignDontCare()
    io.mem.memAddr := 0
    io.mem.memWriteData := 0

    io.mem.memWriteEnable := isValid && DECODE.memWriteEnable
    io.mem.memEnable := isValid && DECODE.memOp
    io.mem.memMask := DECODE.memMask

    val flush = False
    for (stage <- List(genPc, fetch, decode)) {
      stage.throwWhen(flush, usingReady = (stage == genPc))
    }

    when(DECODE.rType) {
      alu.io.src1 := regFile.io.rs1Data
      alu.io.src2 := regFile.io.rs2Data
      WRITE_BACK_VALUE := alu.io.dst
    }

    when(DECODE.iType) {
      alu.io.src1 := regFile.io.rs1Data
      alu.io.src2 := DECODE.imm
      WRITE_BACK_VALUE := alu.io.dst // ignored for loads

      when(DECODE.memOp) {
        io.mem.memAddr := alu.io.dst.resized
      }

      when(isValid && DECODE.branchType) {
        pcArea.pc := alu.io.dst.resized
        flush := True

        WRITE_BACK_VALUE := (PC + 4).resized
      }
    }

    when(DECODE.sType) {
      alu.io.src1 := regFile.io.rs1Data
      alu.io.src2 := DECODE.imm

      io.mem.memAddr := alu.io.dst.resized
      io.mem.memWriteData := regFile.io.rs2Data
    }

    when(DECODE.jType) {
      alu.io.src1 := regFile.io.rs1Data
      alu.io.src2 := PC + DECODE.imm

      when(isValid) {
        // Set PC to branch target and flush pipe
        pcArea.pc := alu.io.dst.resized
        flush := True

        // Link register
        WRITE_BACK_VALUE := (PC + 4).resized
        report(Seq("jumping to PC ", alu.io.dst.resized))
      }
    }

    when(DECODE.bType) {
      alu.io.src1 := regFile.io.rs1Data
      alu.io.src2 := regFile.io.rs2Data

      when(isValid && alu.io.dst(0)) {
        // hack
        val newPc = (PC + DECODE.imm).resized
        pcArea.pc := newPc
        flush := True
        report(Seq("branching to PC ", newPc))
      }
    }

    when(DECODE.uType) {
      when(DECODE.pcRel) {
        // todo: use ALU ?
        WRITE_BACK_VALUE := (PC + DECODE.imm).resized
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

    regFile.io.rd := DECODE.rd
    regFile.io.writeValid := isValid && (DECODE.rType || DECODE.iType || DECODE.jType || DECODE.uType)
    io.error := RegNextWhen(True, isValid && DECODE.error) init False

    val order = Counter(64 bits)

    when(isValid) {
      order.increment()
    }

    io.rvfi_valid := isValid
    io.rvfi_insn := DECODE.inst.asUInt
    io.rvfi_order := order
    io.rvfi_halt := DECODE.error
    io.rvfi_pc := PC
  }

  Builder(genPc, pc2f, fetch, f2d, decode, d2e, execute, e2w, writeback)
}

object CoreVerilog extends App {
  Config.synth.generateVerilog(new Core)
}
