package nutcore

import chisel3._
import chisel3.util._

object DasicsInstr extends HasInstrType {
  def PULPRET      = BitPat("b?????????????????111?????0001011")

  val table = Array(
    PULPRET        -> List(InstrI, FuType.bru, ALUOpType.pulpret)
  )
}