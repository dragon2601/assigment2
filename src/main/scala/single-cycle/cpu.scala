// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu

import chisel3._
import chisel3.util._
import dinocpu.components._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.4 of Patterson and Hennessy
 * This follows figure 4.21
 */
class SingleCycleCPU(implicit val conf: CPUConfig) extends BaseCPU {
  // All of the structures required
  val pc              = dontTouch(RegInit(0.U(64.W)))
  val control         = Module(new Control())
  val registers       = Module(new RegisterFile())
  val aluControl      = Module(new ALUControl())
  val alu             = Module(new ALU())
  val immGen          = Module(new ImmediateGenerator())
  val controlTransfer = Module(new ControlTransferUnit())
  val (cycleCount, _) = Counter(true.B, 1 << 30)

  

  registers.io := DontCare
  aluControl.io := DontCare
  alu.io := DontCare
  immGen.io := DontCare
  controlTransfer.io := DontCare
  io.dmem <> DontCare

  //FETCH
  io.imem.address := pc
  io.imem.valid := true.B
  controlTransfer.io.imm := immGen.io.sextImm

  /*PC */
  val instruction = Wire(UInt(32.W))
  when ((pc % 8.U) === 4.U) {
    instruction := io.imem.instruction(63, 32)
  } .otherwise {
    instruction := io.imem.instruction(31, 0)
  }
  controlTransfer.io.pc := pc
  // Your code goes here
  
  

  /*Instruction Memory*/
  control.io.opcode := instruction(6,0)
  registers.io.readreg1 := instruction(19,15)
  registers.io.readreg2 := instruction(24,20)
  registers.io.writereg := instruction(11,7)
  aluControl.io.funct7 := instruction(31,25)
  aluControl.io.funct3 := instruction(14,12)
  controlTransfer.io.funct3 := instruction(14,12)
  immGen.io.instruction := instruction
  
  

  /*Control Unit*/
  aluControl.io.aluop := control.io.aluop
  when(instruction(11,7) === 0.U){
    registers.io.wen := 0.U
  } 
  .otherwise{
    registers.io.wen := control.io.writeback_valid
  }
  controlTransfer.io.controltransferop := control.io.controltransferop 
  when(control.io.op1_src===0.U){
    alu.io.operand1 := registers.io.readdata1
  }
  .elsewhen(control.io.op1_src===1.U){
    alu.io.operand1 := pc
  }
  
  /*ControlTransferUnit*/
  pc := controlTransfer.io.nextpc

  /*ALU Control*/
  //when(control.io.validinst === 1.U){
  alu.io.operation := aluControl.io.operation
  //}
  


  /*Register File*/
  controlTransfer.io.operand1 := registers.io.readdata1
  when(control.io.op2_src===0.U){
    controlTransfer.io.operand2 := registers.io.readdata2
    alu.io.operand2 := registers.io.readdata2
  }
  .elsewhen(control.io.op2_src===1.U){
    controlTransfer.io.operand2 := 4.U
    alu.io.operand2 := 4.U
  }
  .elsewhen(control.io.op2_src===2.U){
    controlTransfer.io.operand2 := immGen.io.sextImm 
    alu.io.operand2 := immGen.io.sextImm 
  }



  /*ALU */
  when(control.io.writeback_src===0.U){
    registers.io.writedata := alu.io.result 
  }
  .elsewhen(control.io.writeback_src===1.U){
    registers.io.writedata := immGen.io.sextImm 
  }
  .elsewhen(control.io.writeback_src===2.U){
    registers.io.writedata := io.dmem.readdata//check this 
  }
  io.dmem.address := alu.io.result;

  /*DMEM*/
  io.dmem.valid := 1.U
  when(control.io.memop === 0.U){
    io.dmem.memread := 0.U
    io.dmem.memwrite := 0.U
  }
  .elsewhen(control.io.memop === 1.U){
    io.dmem.memread := 1.U
    io.dmem.memwrite := 0.U
  }
  .elsewhen(control.io.memop === 2.U){
    io.dmem.memread := 0.U
    io.dmem.memwrite := 1.U
  }
}



/*
 * Object to make it easier to print information about the CPU
 */
object SingleCycleCPUInfo {
  def getModules(): List[String] = {
    List(
      "dmem",
      "imem",
      "control",
      "registers",
      "csr",
      "aluControl",
      "alu",
      "immGen",
      "controlTransfer"
    )
  }
}
