// Logic to calculate the next pc

package dinocpu.components

import chisel3._

/**
 * ControlTransfer Unit.
 * This component takes care of calculating/deciding the PC of the next cycle upon a control transfer instruction (jump/branch-type).
 *
 * Input: controltransferop        Specifying the type of control transfer instruction (J-type/B-type)
 *                                          . 0 for none of the below
 *                                          . 1 for jal
 *                                          . 2 for jalr
 *                                          . 3 for branch instructions (B-type)
 * Input: operand1                 First input
 * Input: operand2                 Second input
 * Input: funct3                   The funct3 from the instruction
 * Input: pc                       The *current* program counter for this instruction
 * Input: imm                      The sign-extended immediate
 *
 * Output: nextpc                  The address of the next instruction
 * Output: taken                   True if, either the instruction is a branch instruction and it is taken, or it is a jump instruction
 *
 */
class ControlTransferUnit extends Module {
  val io = IO(new Bundle {
    val controltransferop = Input(UInt(2.W))
    val operand1          = Input(UInt(64.W))
    val operand2          = Input(UInt(64.W))
    val funct3            = Input(UInt(3.W))
    val pc                = Input(UInt(64.W))
    val imm               = Input(UInt(64.W))
  
    val nextpc   = Output(UInt(64.W))
    val taken    = Output(Bool())
  })

  // default case, i.e., non-control-transfer instruction, or non-taken branch
  io.taken := false.B
  io.nextpc := io.pc + 4.U

  //None
  when(io.controltransferop === 0.U){
     io.taken := false.B
     io.nextpc := io.pc + 4.U
  }
  .elsewhen(io.controltransferop === 3.U){
    //Equal
    when(io.funct3 === 0.U){
      when(io.operand1.asSInt === io.operand2.asSInt){
        io.nextpc := io.pc + io.imm
        io.taken := true.B
      }
      .otherwise{
        io.taken := false.B
        io.nextpc := io.pc + 4.U
      }
    }
    //Not Equal
    .elsewhen(io.funct3 === 1.U){
      when(io.operand1.asSInt === io.operand2.asSInt){
        io.nextpc := io.pc + 4.U
        io.taken := false.B
      }
      .otherwise{
        io.taken := true.B
        io.nextpc := io.pc + io.imm
      }
    }
    // Less Than
    .elsewhen(io.funct3 === 4.U){
      when(io.operand1.asSInt < io.operand2.asSInt){
        io.taken := true.B
        io.nextpc := io.pc + io.imm
      }
      .otherwise{
        io.taken := false.B
        io.nextpc := io.pc + 4.U
      }
    }
    // Greater Or Equal
    .elsewhen(io.funct3 === 5.U){
      when(io.operand1.asSInt >= io.operand2.asSInt){
        io.taken := true.B
        io.nextpc := io.pc + io.imm
      }
      .otherwise{
        io.taken := false.B
        io.nextpc := io.pc + 4.U
      }
    }
    // Less Than (U)
    .elsewhen(io.funct3 === 6.U){
      when(io.operand1 < io.operand2){
        io.taken := true.B
        io.nextpc := io.pc + io.imm
      }
      .otherwise{
        io.taken := false.B
        io.nextpc := io.pc + 4.U
      }
    } //Greater Or Equal (U)
    .elsewhen(io.funct3 === 7.U){
      when(io.operand1 >= io.operand2){
        io.taken := true.B
        io.nextpc := io.pc + io.imm
      }
      .otherwise{
        io.taken := false.B
        io.nextpc := io.pc + 4.U
      }
    }
  }
  .elsewhen(io.controltransferop === 1.U){
    io.nextpc := io.pc + io.imm
    io.taken := true.B
  }
  .elsewhen(io.controltransferop === 2.U){
    io.nextpc := io.operand1 + io.imm
    io.taken := true.B
  }
}
