package eepyu

import spinal.core._
import spinal.core.sim._

import org.scalatest.funsuite._
import com.carlosedp.riscvassembler.RISCVAssembler

import spinal.lib.misc.pipeline._

import net.fornwall.jelf.ElfFile
import java.io.File
import net.fornwall.jelf.ElfSegment
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import java.io.RandomAccessFile
import net.fornwall.jelf.ElfSectionHeader

class CoreTests extends AnyFunSuite {
  def assemble(inst: String) = BigInt(RISCVAssembler.binOutput(inst), 2)

  def withAssembledProgramMemory(dut: Core, imem: Seq[String]) = {
    withProgramMemory(dut, imem.map(assemble), Seq())
  }

  def withProgramMemory(dut: Core, imem: Seq[BigInt], dmemIn: Seq[BigInt]) = {
    dut.clockDomain.forkStimulus(2)

    val imemThread = fork {
      while (true) {
        // Sync mem
        dut.clockDomain.waitSampling(1)
        val addr = dut.io.mem.imemReadAddr.toInt / 4
        val data = imem.lift(addr).getOrElse(BigInt(0))
        dut.io.mem.imemReadData #= data
      }
    }

    val dmemThread = fork {
      val dmem = Map.empty[Int, Long]
      for ((word, idx) <- dmemIn.zipWithIndex) {
        dmem(idx * 4) = word.toLong
      }

      while (true) {
        dut.clockDomain.waitSamplingWhere(dut.io.mem.memEnable.toBoolean)

        var unalignedAddr = dut.io.mem.memAddr.toInt
        unalignedAddr -= 0x1000 // hack
        val alignedAddr = unalignedAddr & ~3
        val misalignment = unalignedAddr - alignedAddr

        def expandMask(memType: MemType.E) = {
          memType match {
            case MemType.Byte | MemType.ByteU         => 0xff
            case MemType.HalfWord | MemType.HalfWordU => 0xffff
            case MemType.Word                         => 0xffffffff
          }
        }
        def doSext(x: Long, memType: MemType.E) = {
          val ext = memType match {
            case MemType.Byte     => x.toByte.toInt
            case MemType.HalfWord => x.toShort.toInt
            case default          => x
          }

          if (ext < 0) {
            0xffffffffL + ext + 1
          } else {
            ext
          }
        }
        val mask = expandMask(dut.io.mem.memType.toEnum)

        // TESTS ARE FAILING BECAUSE LB DOESN'T SIGN EXTEND !!!!!!!!!!!!!!!!

        if (dut.io.mem.memWriteEnable.toBoolean) {
          // hack: we initialise memory to zero if first write is sub-word
          val existing: Long = dmem.lift(alignedAddr).getOrElse(0xccccccccL)
          val toWrite = dut.io.mem.memWriteData.toLong << (8 * misalignment)
          val shiftedMask = mask << (8 * misalignment)
          val updated = (existing & ~shiftedMask) | (toWrite & shiftedMask)
          // println(
          //   f"Wrote address $unalignedAddr%x with mask $mask%x (before = $existing%x, new = $toWrite%x, after = $updated%x)"
          // )

          dmem(alignedAddr) = updated
        } else {
          assert(dmem.contains(alignedAddr))

          val word = dmem(alignedAddr)
          val shifted = word >> (8 * misalignment)
          val masked = shifted & mask
          val sext = doSext(masked, dut.io.mem.memType.toEnum)
          // println(f"Read address $unalignedAddr%x with mask $mask%x (word = $word%x, shifted/masked = $masked%x, sext = $sext%x)")

          dut.io.mem.memReadData #= sext
        }
      }
    }

    val retireMonitor = fork {
      while (true) {
        dut.clockDomain.waitSamplingWhere(dut.io.rvfi_valid.toBoolean)
        val inst = dut.io.rvfi_insn.toBigInt
        // println(f"Retired instruction $inst%x")
      }
    }
  }

  def nextRetiredInstruction(dut: Core) = {
    dut.clockDomain.waitSamplingWhere(dut.io.rvfi_valid.toBoolean)
    val inst = dut.io.rvfi_insn.toBigInt
    // println(f"Retired instruction $inst%x")
    inst
  }

  val compiled = Config.sim.compile(new Core(16, 16))

  test("should trap on illegal instruction") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.io.mem.imemReadData #= 0
      dut.io.mem.memReadData #= 0
      assert(!dut.io.error.toBoolean)
      dut.clockDomain.waitSampling(5)
      sleep(0)
      assert(dut.io.error.toBoolean)
    }
  }

  test("should not trap on add instructions") {
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.io.mem.imemReadData #= 0
      dut.io.mem.memReadData #= 0

      assert(!dut.io.error.toBoolean)
      dut.clockDomain.waitSampling(1)

      dut.io.mem.imemReadData #= assemble("add x0, x1, x2")
      dut.io.mem.memReadData #= 0

      for (i <- 0 until 10) {
        assert(!dut.io.error.toBoolean)
        dut.clockDomain.waitSampling(1)
      }
    }
  }

  test("should writeback to registers") {
    val program =
      Seq(
        "addi x1, x0, 1",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0",
        "sub x0, x0, x0"
      )
    compiled.doSim { dut =>
      withAssembledProgramMemory(dut, program)
      dut.clockDomain.waitSamplingWhere(15)(dut.regFile.rs1mem.getBigInt(1) == 1)
    }
  }

  test("should retire instructions in order") {
    val program =
      Seq(
        "addi x0, x0, 0",
        "addi x0, x0, 1",
        "addi x0, x0, 2",
        "addi x0, x0, 3",
        "addi x0, x0, 4",
        "addi x0, x0, 5",
        "addi x0, x0, 6",
        "addi x0, x0, 7"
      )
    compiled.doSim { dut =>
      withAssembledProgramMemory(dut, program)
      assert(nextRetiredInstruction(dut) == assemble(program(0)))
      assert(nextRetiredInstruction(dut) == assemble(program(1)))
      assert(nextRetiredInstruction(dut) == assemble(program(2)))
      assert(nextRetiredInstruction(dut) == assemble(program(3)))
      assert(nextRetiredInstruction(dut) == assemble(program(4)))
      assert(nextRetiredInstruction(dut) == assemble(program(5)))
      assert(nextRetiredInstruction(dut) == assemble(program(6)))
      assert(nextRetiredInstruction(dut) == assemble(program(7)))
    }
  }

  test("should jump") {
    val program =
      Seq(
        "addi x1, x0, 1",
        "sub x0, x0, x0",
        "jal x2, -8",
        "xori x0, x0, 0",
        "xori x0, x0, 0"
      )
    compiled.doSim { dut =>
      withAssembledProgramMemory(dut, program)

      assert(nextRetiredInstruction(dut) == assemble(program(0)))
      assert(nextRetiredInstruction(dut) == assemble(program(1)))
      assert(nextRetiredInstruction(dut) == assemble(program(2)))

      assert(nextRetiredInstruction(dut) == assemble(program(0)))
      assert(nextRetiredInstruction(dut) == assemble(program(1)))
      assert(nextRetiredInstruction(dut) == assemble(program(2)))
    }
  }

  test("should branch") {
    val program =
      Seq(
        "addi x1, x0, 1",
        "addi x2, x0, 3",
        "addi x1, x1, 1",
        "blt x1, x2, -4",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0"
      )
    compiled.doSim { dut =>
      withAssembledProgramMemory(dut, program)

      assert(nextRetiredInstruction(dut) == assemble(program(0)))
      assert(nextRetiredInstruction(dut) == assemble(program(1)))
      assert(nextRetiredInstruction(dut) == assemble(program(2)))
      assert(nextRetiredInstruction(dut) == assemble(program(3)))

      assert(nextRetiredInstruction(dut) == assemble(program(2)))
      assert(nextRetiredInstruction(dut) == assemble(program(3)))

      assert(nextRetiredInstruction(dut) == assemble(program(4)))
      assert(nextRetiredInstruction(dut) == assemble(program(5)))
      assert(nextRetiredInstruction(dut) == assemble(program(6)))
    }
  }

  test("hazard") {
    val program =
      Seq(
        "addi x1, x0, 5",
        "addi x2, x0, 3",
        "sub x3, x1, x2",
        "xori x0, x0, 0",
        "addi x6, x0, 0",
        "add x6, x0, x6",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "unimp"
      )
    compiled.doSim { dut =>
      withAssembledProgramMemory(dut, program)
      dut.clockDomain.waitSampling(15)

      assert(dut.regFile.rs1mem.getBigInt(1) == 5)
      assert(dut.regFile.rs1mem.getBigInt(2) == 3)
      assert(dut.regFile.rs1mem.getBigInt(3) == 2)
    }
  }

  test("hazard2") {
    val program =
      Seq(
        "addi x1, x0, 5",
        "add x1, x1, x1",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "xori x0, x0, 0",
        "unimp"
      )
    compiled.doSim { dut =>
      withAssembledProgramMemory(dut, program)
      dut.clockDomain.waitSampling(10)

      assert(dut.regFile.rs1mem.getBigInt(1) == 10)
    }
  }

  def runRiscvTest(name: String) = {
    val imem = ArrayBuffer.empty[BigInt]
    val dmem = ArrayBuffer.empty[BigInt]

    val file = new File("../../opt/riscv-tests/isa/" + name)
    val rafile = new RandomAccessFile(file, "r")
    val elf = ElfFile.from(file)

    for (i <- 0 until elf.e_phnum) {
      val segment = elf.getProgramHeader(i)
      if ((segment.p_type & ElfSegment.PT_LOAD) != 0) {
        for (addr <- 0L until segment.p_filesz by 4) {
          rafile.seek(segment.p_offset + addr)
          val bytes = Seq(
            rafile.readUnsignedByte(),
            rafile.readUnsignedByte(),
            rafile.readUnsignedByte(),
            rafile.readUnsignedByte()
          )
          val word: BigInt = (BigInt(bytes(3)) << 24) | (BigInt(bytes(2)) << 16) | (BigInt(bytes(1)) << 8) | bytes(0)

          if ((segment.p_flags & 1) != 0) {
            imem += word
          } else if ((segment.p_flags & 2) != 0) {
            // println(f"Loaded word $word%x at dmem address $addr%x")
            dmem += word
          }
        }
      }
    }

    compiled.doSim { dut =>
      SimTimeout(5000)
      withProgramMemory(dut, imem.toSeq, dmem.toSeq)
      dut.clockDomain.waitSamplingWhere(dut.io.rvfi_valid.toBoolean && dut.io.rvfi_halt.toBoolean)
      val gp = dut.regFile.rs1mem.getBigInt(3)
      assert(dut.io.rvfi_insn.toBigInt != 2, s": test number $gp failed")
      assert(dut.io.rvfi_insn.toBigInt == 1, ": unexpected exit code")
    }
  }

  val tests = Seq(
    "rv32ui-p-add",
    "rv32ui-p-addi",
    "rv32ui-p-and",
    "rv32ui-p-andi",
    "rv32ui-p-auipc",
    "rv32ui-p-beq",
    "rv32ui-p-bge",
    "rv32ui-p-bgeu",
    "rv32ui-p-blt",
    "rv32ui-p-bltu",
    "rv32ui-p-bne",
    // "rv32ui-p-fence_i", // Self-modifying code is not supported.
    "rv32ui-p-jal",
    "rv32ui-p-jalr",
    "rv32ui-p-lb",
    "rv32ui-p-lbu",
    "rv32ui-p-lh",
    "rv32ui-p-lhu",
    "rv32ui-p-lui",
    "rv32ui-p-lw",
    // "rv32ui-p-ma_data", // Misaligned loads and stores are not supported.
    "rv32ui-p-or",
    "rv32ui-p-ori",
    "rv32ui-p-sb",
    "rv32ui-p-sh",
    "rv32ui-p-simple",
    "rv32ui-p-sll",
    "rv32ui-p-slli",
    "rv32ui-p-slt",
    "rv32ui-p-slti",
    "rv32ui-p-sltiu",
    "rv32ui-p-sltu",
    "rv32ui-p-sra",
    "rv32ui-p-srai",
    "rv32ui-p-srl",
    "rv32ui-p-srli",
    "rv32ui-p-sub",
    "rv32ui-p-sw",
    "rv32ui-p-xor",
    "rv32ui-p-xori"
  )

  for (testName <- tests) {
    test(testName) {
      runRiscvTest(testName)
    }
  }
}
