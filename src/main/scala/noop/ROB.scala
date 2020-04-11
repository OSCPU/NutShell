package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

object physicalRFTools{
  def getPRFAddr(robIndex: UInt, bank: UInt): UInt = {
    Cat(robIndex, bank(0))
  }
}

class ROB(implicit val p: NOOPConfig) extends NOOPModule with HasInstrType with HasBackendConst with HasRegFileParameter{
  val io = IO(new Bundle {
    val in = Vec(robWidth, Flipped(Decoupled(new DecodeIO)))
    val brMaskIn = Input(Vec(robWidth, UInt(robInstCapacity.W)))
    val cdb = Vec(robWidth, Flipped(Valid(new OOCommitIO)))
    val wb = Vec(robWidth, new WriteBackIO)
    val redirect = new RedirectIO
    val flush = Input(Bool())
    val empty = Output(Bool())
    val index = Output(UInt(log2Up(robSize).W))

    // to CSR
    val exception = Output(Bool())
    val beUop = Output(new RenamedDecodeIO)

    // to LSU
    val scommit = Output(Bool())

    // PRF
    val aprf = Output(Vec(robWidth * 2, UInt(prfAddrWidth.W))) // prf addr for current enqueue inst
    val rprf = Output(Vec(robWidth * 2, UInt(XLEN.W))) // prf value data for current enqueue inst
    val rvalid = Output(Vec(robWidth * 2, Bool()))
    val rcommited = Output(Vec(robWidth * 2, Bool()))

    // Misprediction recovery
    val brMaskClearVec = Output(UInt(robInstCapacity.W))
  })

  def needMispredictionRecovery(brMask: UInt) = {
    List.tabulate(CommitWidth)(i => (io.cdb(i).bits.decode.cf.redirect.valid && (io.cdb(i).bits.decode.cf.redirect.rtype === 1.U) && brMask(io.cdb(i).bits.prfidx))).foldRight(false.B)((sum, i) => sum | i)
  }

  def updateBrMask(brMask: UInt) = {
    brMask & ~ List.tabulate(CommitWidth)(i => (UIntToOH(io.cdb(i).bits.prfidx) & Fill(robInstCapacity, io.cdb(i).valid))).foldRight(0.U)((sum, i) => sum | i)
  }

  val decode = Reg(Vec(robSize, Vec(robWidth, new DecodeIO)))
  val brMask = RegInit(VecInit(List.fill(robSize)(VecInit(List.fill(robWidth)(0.U(robInstCapacity.W))))))
  val valid = RegInit(VecInit(List.fill(robSize)(VecInit(List.fill(robWidth)(false.B)))))
  // val valid = List.fill(robWidth)(Mem(robSize, Bool()))
  // val validReg = RegInit(0.U.asTypeOf(Vec(robSize * robWidth, Bool())))
  val commited = Reg(Vec(robSize, Vec(robWidth, Bool())))
  val canceled = Reg(Vec(robSize, Vec(robWidth, Bool())))
  val redirect = Reg(Vec(robSize, Vec(robWidth, new RedirectIO)))
  val exception = Reg(Vec(robSize, Vec(robWidth, Bool()))) // Backend exception
  val isMMIO = Reg(Vec(robSize, Vec(robWidth, Bool())))
  val intrNO = Reg(Vec(robSize, Vec(robWidth, UInt(XLEN.W))))
  val prf = Mem(robSize * robWidth, UInt(XLEN.W))

  // def valid(i: UInt, j: UInt){
  //   validReg(Cat(i.UInt(log2Up(robSize)), j.UInt(log2Up(robWidth))))
  // }

  // Almost all int/exceptions can be detected at decode stage, excepting for load/store related exception.
  // In NOOP-Argo's backend, non-l/s int/exc will be sent dircetly to CSR when dispatch,
  // while l/s exc will be sent to CSR by LSU.

  val ringBufferHead = RegInit(0.U(log2Up(robSize).W))
  val ringBufferTail = RegInit(0.U(log2Up(robSize).W))
  val ringBufferEmpty = ringBufferHead === ringBufferTail && !valid(ringBufferHead)(0) && !valid(ringBufferHead)(1)
  val ringBufferFull = ringBufferTail === ringBufferHead && (valid(ringBufferHead)(0) || valid(ringBufferHead)(1))
  val ringBufferAllowin = !ringBufferFull 

  def forAllROBBanks(func: Int => _) = {
    List.tabulate(robWidth)(func)
  }

  io.index := ringBufferHead
  io.empty := ringBufferEmpty

  // Register Map  
  val rmtMap = Mem(NRReg, UInt(prfAddrWidth.W))
  val rmtValid = RegInit(VecInit(Seq.fill(NRReg)(false.B)))
  // val rmtCommited = RegInit(VecInit(Seq.fill(NRReg)(false.B)))

  forAllROBBanks((i: Int) => {
    io.aprf(2*i)        := rmtMap(io.in(i).bits.ctrl.rfSrc1)
    io.aprf(2*i+1)      := rmtMap(io.in(i).bits.ctrl.rfSrc2)
    io.rprf(2*i)        := prf(io.aprf(2*i))
    io.rprf(2*i+1)      := prf(io.aprf(2*i+1))
    io.rvalid(2*i)      := rmtValid(io.in(i).bits.ctrl.rfSrc1)
    io.rvalid(2*i+1)    := rmtValid(io.in(i).bits.ctrl.rfSrc2)
    io.rcommited(2*i)   := commited(io.aprf(2*i)>>1)(io.aprf(2*i)(0)) && !canceled(io.aprf(2*i)>>1)(io.aprf(2*i)(0))
    io.rcommited(2*i+1) := commited(io.aprf(2*i+1)>>1)(io.aprf(2*i+1)(0)) && !canceled(io.aprf(2*i+1)>>1)(io.aprf(2*i+1)(0))
  })

  //---------------------------------------------------------
  // Commit logic
  //---------------------------------------------------------

  // commit to ROB
  // ROB listens to CDB (common data bus), i.e. CommitIO
  // An ROB term will be marked as commited after that inst was commited to CDB
  // This will always success

  // if ROB index == commit index && bank index == commit bank index
  for(i <- (0 to robSize - 1)){
    for(j <- (0 to robWidth - 1)){
      val robIdx = Cat(i.asUInt(log2Up(robSize).W), j.asUInt(log2Up(robWidth).W))
      for(k <- (0 to robWidth - 1)){
        when(valid(i)(j) && io.cdb(k).bits.prfidx === robIdx && io.cdb(k).valid){
        // when(true.B){
          when(commited(i)(j)){
            printf("[ERROR] double commit at time %d robidx %d pc %x inst %x pcin %x instin %x\n", GTimer(), robIdx, decode(i)(j).cf.pc, decode(i)(j).cf.instr, io.cdb(k).bits.decode.cf.pc, io.cdb(k).bits.decode.cf.instr)
          }
          when(io.cdb(k).bits.decode.cf.pc =/= decode(i)(j).cf.pc){
            printf("[ERROR] commit pc not match at time %d robidx %d pc %x inst %x pcin %x instin %x\n", GTimer(), robIdx, decode(i)(j).cf.pc, decode(i)(j).cf.instr, io.cdb(k).bits.decode.cf.pc, io.cdb(k).bits.decode.cf.instr)
          }
          assert(io.cdb(k).bits.decode.cf.pc === decode(i)(j).cf.pc)
          assert(!commited(i)(j), "double commit")
          // Mark an ROB term as commited
          when(!io.cdb(k).bits.exception){
            commited(i)(j) := true.B
          }
          // Write result to ROB-PRF
          prf(robIdx) := io.cdb(k).bits.commits
          // Write other info which could be generated by function units
          isMMIO(i)(j) := io.cdb(k).bits.isMMIO
          intrNO(i)(j) := io.cdb(k).bits.intrNO
          // Write redirect info
          redirect(i)(j) := io.cdb(k).bits.decode.cf.redirect
          redirect(i)(j).valid := io.cdb(k).bits.decode.cf.redirect.valid
          exception(i)(j) := io.cdb(k).bits.exception
          // Update wen
          // In several cases, FU will invalidate rfWen
          decode(i)(j).ctrl.rfWen := io.cdb(k).bits.decode.ctrl.rfWen
        }
        when(valid(i)(j) && brMask(i)(j)(io.cdb(k).bits.prfidx) && io.cdb(k).valid && io.cdb(k).bits.decode.cf.redirect.valid ){
          valid(i)(j) := false.B
          canceled(i)(j) := true.B
          Debug(){
            printf("[MB] %d: robIdx %d %x (%b) reset by %d %x\n", GTimer(), robIdx, decode(i)(j).cf.pc, brMask(i)(j), io.cdb(k).bits.prfidx, io.cdb(k).bits.decode.cf.pc)
          }
        }
      }
      brMask(i)(j) := updateBrMask(brMask(i)(j) & ~io.brMaskClearVec)
    }
  }

  //---------------------------------------------------------
  // Retire logic
  //---------------------------------------------------------

  // ROB Retire
  // We write back at most #bank reg results back to arch-rf.
  // Then we mark those ROB terms as finished, i.e. `!valid`
  // No more than robWidth insts can retire from ROB in a single cycle.
  val tailBankNotUsed = List.tabulate(robWidth)(i => !valid(ringBufferTail)(i) || valid(ringBufferTail)(i) && commited(ringBufferTail)(i))
  val tailTermEmpty = List.tabulate(robWidth)(i => !valid(ringBufferTail)(i)).foldRight(true.B)((sum, i) => sum & i)
  // TODO: refactor retire logic
  val skipException = valid(ringBufferTail)(0) && redirect(ringBufferTail)(0).valid && commited(ringBufferTail)(0) && exception(ringBufferTail)(1)
  val retireATerm = tailBankNotUsed.foldRight(true.B)((sum, i) => sum & i) && !tailTermEmpty || skipException
  val recycleATerm = tailTermEmpty && (ringBufferTail =/= ringBufferHead)
  when(retireATerm || recycleATerm){
    forAllROBBanks((i: Int) =>{
      valid(ringBufferTail)(i) := false.B
      // free prf (update RMT)
      when(
        decode(ringBufferTail)(i).ctrl.rfWen && 
        rmtMap(decode(ringBufferTail)(i).ctrl.rfDest) === Cat(ringBufferTail, i.U(1.W)) && // no other in flight inst will write this reg 
        (valid(ringBufferTail)(i) || canceled(ringBufferTail)(i))
      ){
        //TODO: make it prettier
        rmtValid(decode(ringBufferTail)(i).ctrl.rfDest) := false.B
      }
    })
    ringBufferTail := ringBufferTail + 1.U
  }

  //FIXIT: for map table checkpoints
  val enableMapTableCheckpoint = false
  if(enableMapTableCheckpoint){
    io.brMaskClearVec := 
      UIntToOH(Cat(ringBufferTail, "b0".U)) & Fill(robInstCapacity, (valid(ringBufferTail)(0) || canceled(ringBufferTail)(0)) && (retireATerm || recycleATerm)) |
      UIntToOH(Cat(ringBufferTail, "b1".U)) & Fill(robInstCapacity, (valid(ringBufferTail)(1) || canceled(ringBufferTail)(1)) && (retireATerm || recycleATerm))
  } else {
    io.brMaskClearVec := 0.U
  }

  // retire: trigger redirect
  // exception/interrupt/branch mispredict redirect is raised by ROB
  val redirectBank = Mux(redirect(ringBufferTail)(0).valid && valid(ringBufferTail)(0), 0.U, 1.U) // TODO: Fix it for robWidth > 2
  io.redirect := redirect(ringBufferTail)(redirectBank)
  io.redirect.valid := retireATerm && List.tabulate(robWidth)(i => 
    redirect(ringBufferTail)(i).valid && valid(ringBufferTail)(i)
  ).foldRight(false.B)((sum, i) => sum || i)
  // io.redirect.rtype := 0.U
  // TODO: redirect when inst1 is valid

  // retire: trigger exception
  // Only l/s exception will trigger ROB flush
  // Other exception/interrupt will be stalled by Dispatch Unit until ROB is empty

  // Currently, AGU(LSU) will send exception to CDB and commit this inst.
  // However, commit with backend exception will not set commited to `true`.
  // When ROB detected a backend exception at the tail of ROB, a `trigger exception`
  // signal will be sent to CSR. CSR gets info about this inst from a special reg.
  // The inst which caused exception will be commited by CSR.

  // By doing this, we can treat CSR module as a single cycle FU.

  // TODO: delay 1 cycle for better timing performance
  io.exception := 
    valid(ringBufferTail)(0) && exception(ringBufferTail)(0) ||
    valid(ringBufferTail)(1) && exception(ringBufferTail)(1) && (!valid(ringBufferTail)(0) || commited(ringBufferTail)(0) && !redirect(ringBufferTail)(0).valid)

  // setup beUop for CSR
  // `beUop` stands for `backend exception uop`
  val exceptionSelect = Mux(exception(ringBufferTail)(0), 0.U, 1.U)
  io.beUop := DontCare
  io.beUop.decode := decode(ringBufferTail)(exceptionSelect)
  for(i <- 0 to storePageFault){io.beUop.decode.cf.exceptionVec(i) := false.B}
  io.beUop.decode.cf.exceptionVec(loadPageFault) := intrNO(ringBufferTail)(exceptionSelect)(loadPageFault)
  io.beUop.decode.cf.exceptionVec(storePageFault) := intrNO(ringBufferTail)(exceptionSelect)(storePageFault)
  io.beUop.decode.cf.exceptionVec(loadAddrMisaligned) := intrNO(ringBufferTail)(exceptionSelect)(loadAddrMisaligned)
  io.beUop.decode.cf.exceptionVec(storeAddrMisaligned) := intrNO(ringBufferTail)(exceptionSelect)(storeAddrMisaligned)
  io.beUop.decode.data.src1 := prf(Cat(ringBufferTail, exceptionSelect)) //FIXIT
  io.beUop.prfDest := Cat(ringBufferTail, exceptionSelect)

  assert(!(exception(ringBufferTail)(0) && exception(ringBufferTail)(1) && valid(ringBufferTail)(0) && valid(ringBufferTail)(1)))
  assert(!(exception(ringBufferTail)(0) && decode(ringBufferTail)(0).ctrl.fuType =/= FuType.lsu && valid(ringBufferTail)(0)))
  assert(!(exception(ringBufferTail)(1) && decode(ringBufferTail)(1).ctrl.fuType =/= FuType.lsu && valid(ringBufferTail)(1)))

  // Raise decoupled store request
  // If l/s are decoupled, store request is sent to store buffer here.
  // Note: only # of safe store ops is sent to LSU
  io.scommit := List.tabulate(robWidth)(i => 
    valid(ringBufferTail)(i) && 
    decode(ringBufferTail)(i).ctrl.fuType === FuType.lsu && 
    LSUOpType.needMemWrite(decode(ringBufferTail)(i).ctrl.fuOpType) && 
    List.tabulate(i+1)(j => (!redirect(ringBufferTail)(j).valid)).foldRight(true.B)((sum, k) => sum && k) && 
    List.tabulate(i+1)(j => (!exception(ringBufferTail)(j))).foldRight(true.B)((sum, k) => sum && k)
  ).foldRight(false.B)((sum, i) => sum || i) && retireATerm

  Debug(){
    when(io.scommit){
      printf("[SCommit] %x %x %x %x\n", 
        redirect(ringBufferTail)(0).valid,
        redirect(ringBufferTail)(1).valid,
        exception(ringBufferTail)(0),
        exception(ringBufferTail)(1)
      )
    }
  }

  // In current version, only one l/s inst can be sent to agu in a cycle
  // therefore, in all banks, there is no more than 1 store insts

  // Arch-RF write back
  for(i <- (0 to robWidth - 1)){
    // val haveRedirect = List.tabulate(i + 1)(j => redirect(ringBufferTail)(j).valid && valid(ringBufferTail)(j)).foldRight(false.B)((sum, i) => sum|i)
    io.wb(i).rfWen := retireATerm && decode(ringBufferTail)(i).ctrl.rfWen && valid(ringBufferTail)(i)
    io.wb(i).rfDest := decode(ringBufferTail)(i).ctrl.rfDest
    io.wb(i).rfData := prf(Cat(ringBufferTail, i.U))
  }
  // fix wen
  // TODO: parameterize it
  val inst1Redirect = redirect(ringBufferTail)(0).valid && valid(ringBufferTail)(0)
  // val inst2Redirect = redirect(ringBufferTail)(1).valid && valid(ringBufferTail)(1)
  when(inst1Redirect){
    io.wb(1).rfWen := false.B
  }

  //---------------------------------------------------------
  // Dispatch logic
  //---------------------------------------------------------

  // ROB enqueue
  val validEnqueueRequest = List.tabulate(robWidth)(i => io.in(i).valid).foldRight(false.B)((sum,i)=>sum|i) //io.in(0).valid || io.in(1).valid
  when(validEnqueueRequest && ringBufferAllowin){
    ringBufferHead := ringBufferHead + 1.U
    forAllROBBanks((i: Int) => decode(ringBufferHead)(i) := io.in(i).bits)
    forAllROBBanks((i: Int) => brMask(ringBufferHead)(i) := io.brMaskIn(i))
    forAllROBBanks((i: Int) => valid(ringBufferHead)(i) := io.in(i).valid)
    forAllROBBanks((i: Int) => commited(ringBufferHead)(i) := false.B)
    forAllROBBanks((i: Int) => canceled(ringBufferHead)(i) := false.B)
    forAllROBBanks((i: Int) => redirect(ringBufferHead)(i).valid := false.B)
    forAllROBBanks((i: Int) => exception(ringBufferHead)(i) := false.B)
    forAllROBBanks((i: Int) => 
      when(io.in(i).valid && io.in(i).bits.ctrl.rfWen && io.in(i).bits.ctrl.rfDest =/= 0.U){
        rmtMap(io.in(i).bits.ctrl.rfDest) := Cat(ringBufferHead, i.U)
        rmtValid(io.in(i).bits.ctrl.rfDest) := true.B
        // rmtCommited(io.in(i).bits.ctrl.rfDest) := false.B
      }
    )
  }
  forAllROBBanks((i: Int) => io.in(i).ready := ringBufferAllowin)

  // Generate Debug Info
  Debug(){
    when (io.in(0).fire()){printf("[DISPATCH1] TIMER: %d pc = 0x%x inst %x wen %x wdst %x\n", GTimer(), io.in(0).bits.cf.pc, io.in(0).bits.cf.instr, io.in(0).bits.ctrl.rfWen, io.in(0).bits.ctrl.rfDest)}
    when (io.in(1).fire()){printf("[DISPATCH2] TIMER: %d pc = 0x%x inst %x wen %x wdst %x\n", GTimer(), io.in(1).bits.cf.pc, io.in(1).bits.cf.instr, io.in(1).bits.ctrl.rfWen, io.in(1).bits.ctrl.rfDest)}
    when (io.cdb(0).valid){printf("[COMMIT1] TIMER: %d pc = 0x%x inst %x wen %x wdst %x wdata = 0x%x\n", GTimer(), io.cdb(0).bits.decode.cf.pc, io.cdb(0).bits.decode.cf.instr, io.cdb(0).bits.decode.ctrl.rfWen, io.cdb(0).bits.decode.ctrl.rfDest, io.cdb(0).bits.commits)}
    when (io.cdb(1).valid){printf("[COMMIT2] TIMER: %d pc = 0x%x inst %x wen %x wdst %x wdata = 0x%x\n", GTimer(), io.cdb(1).bits.decode.cf.pc, io.cdb(1).bits.decode.cf.instr, io.cdb(1).bits.decode.ctrl.rfWen, io.cdb(1).bits.decode.ctrl.rfDest, io.cdb(1).bits.commits)}
    when (retireATerm && valid(ringBufferTail)(0)) { printf("[RETIRE1] TIMER: %d pc = 0x%x inst %x wen %x wdst %x wdata %x mmio %x intrNO %x\n", GTimer(), decode(ringBufferTail)(0).cf.pc, decode(ringBufferTail)(0).cf.instr, io.wb(0).rfWen, io.wb(0).rfDest, io.wb(0).rfData, isMMIO(ringBufferTail)(0), intrNO(ringBufferTail)(0)) }
    when (retireATerm && valid(ringBufferTail)(1)) { printf("[RETIRE2] TIMER: %d pc = 0x%x inst %x wen %x wdst %x wdata %x mmio %x intrNO %x\n", GTimer(), decode(ringBufferTail)(1).cf.pc, decode(ringBufferTail)(1).cf.instr, io.wb(1).rfWen, io.wb(1).rfDest, io.wb(1).rfData, isMMIO(ringBufferTail)(1), intrNO(ringBufferTail)(1)) }
  }

  Debug(){
    printf("[ROB] time %d\n", GTimer())
    printf("[ROB] ")
    for(i <- 0 to (robSize - 1)){
      when(valid(i)(0) && commited(i)(0)){printf("c")}.elsewhen(valid(i)(0)){printf("v")}.otherwise{printf("-")}
    }
    printf("\n[ROB] ")
    for(i <- 0 to (robSize - 1)){
      when(valid(i)(1) && commited(i)(1)){printf("c")}.elsewhen(valid(i)(1)){printf("v")}.otherwise{printf("-")}
    }
    printf("\n[ROB] ")
    for(i <- 0 to (robSize - 1)){
      when(ringBufferHead === i.U){printf("h")}
      .elsewhen(ringBufferTail === i.U){printf("t")}
      .otherwise{printf(" ")}
    }
    printf("\n")
    printf("[ROB] pc           v w c r e   pc           v w c r e\n")
    for(i <- 0 to (robSize - 1)){
      printf("[ROB] 0x%x %d %d %d %d %d   0x%x %d %d %d %d %d  " + i, 
        decode(i)(0).cf.pc, valid(i)(0), commited(i)(0), canceled(i)(0), redirect(i)(0).valid && valid(i)(0), exception(i)(0),
        decode(i)(1).cf.pc, valid(i)(1), commited(i)(1), canceled(i)(1), redirect(i)(1).valid && valid(i)(1), exception(i)(1)
      )
      when(valid(i)(0) || valid(i)(1)){printf("  valid")}
      when(ringBufferHead === i.U){printf("  head")}
      when(ringBufferTail === i.U){printf("  tail")}
      printf("\n")
    }
    
    for(i <- 0 to (robSize - 1)){
      printf("[ROB] %b %b " + i + "\n", brMask(i)(0), brMask(i)(1))
    }

    // for(i <- 0 until NRReg){
    //   printf("[ROB] prMap %b %d " + i + "\n", rmtValid(i), rmtMap(i))
    // }

    // printf("[RMT INFO]")
    // for(i <- 0 to (NRReg - 1)){
    //  // if(i % 6 == 0)printf("\n")
    //   when(rmtValid(i)){
    //     printf("%d -> %d %b  ", i.U, rmtMap(i), commited(i.U>>1)(i.U(0)))
    //   }
    // }
    // printf("\n")
  }

  val retireMultiTerms = retireATerm && valid(ringBufferTail)(0) && valid(ringBufferTail)(1) && !inst1Redirect
  val firstValidInst = Mux(valid(ringBufferTail)(0), 0.U, 1.U)
  BoringUtils.addSource(retireATerm, "perfCntCondMinstret")
  BoringUtils.addSource(retireMultiTerms, "perfCntCondMultiCommit")
  val retirePC = SignExt(decode(ringBufferTail)(firstValidInst).cf.pc, AddrBits)
  val retirePC2 = SignExt(decode(ringBufferTail)(1).cf.pc, AddrBits)
  
  if (!p.FPGAPlatform) {
    // TODO: fix debug
    BoringUtils.addSource(RegNext(retireATerm), "difftestCommit")
    BoringUtils.addSource(RegNext(retireMultiTerms), "difftestMultiCommit")
    BoringUtils.addSource(RegNext(retirePC), "difftestThisPC")
    // BoringUtils.addSource(RegNext(SignExt(decode(ringBufferTail)(1).cf.pc, AddrBits)), "difftestThisPC2")
    BoringUtils.addSource(RegNext(decode(ringBufferTail)(firstValidInst).cf.instr), "difftestThisINST")
    // BoringUtils.addSource(RegNext(decode(ringBufferTail)(1).cf.instr), "difftestThisINST2")
    BoringUtils.addSource(RegNext(isMMIO(ringBufferTail)(0) && valid(ringBufferTail)(0) || isMMIO(ringBufferTail)(1) && valid(ringBufferTail)(1) && !(valid(ringBufferTail)(0) && redirect(ringBufferTail)(0).valid)), "difftestIsMMIO")
    // BoringUtils.addSource(RegNext(isMMIO(ringBufferTail)(1)), "difftestIsMMIO2")
    BoringUtils.addSource(RegNext(decode(ringBufferTail)(firstValidInst).cf.instr(1,0)=/="b11".U), "difftestIsRVC")
    BoringUtils.addSource(RegNext(decode(ringBufferTail)(1).cf.instr(1,0)=/="b11".U), "difftestIsRVC2")
    BoringUtils.addSource(RegNext(intrNO(ringBufferTail)(firstValidInst)), "difftestIntrNO")
    // BoringUtils.addSource(RegNext(intrNO(ringBufferTail)(1)), "difftestIntrNO2")
  } else {
    BoringUtils.addSource(DontCare, "ilaWBUvalid")
    BoringUtils.addSource(DontCare, "ilaWBUpc")
    BoringUtils.addSource(DontCare, "ilaWBUrfWen")
    BoringUtils.addSource(DontCare, "ilaWBUrfDest")
    BoringUtils.addSource(DontCare, "ilaWBUrfData")
  }

  // flush control
  when(io.flush){
    ringBufferHead := 0.U
    ringBufferTail := 0.U
    List.tabulate(robSize)(i => valid(i)(0) := 0.U) // set valid to 0
    List.tabulate(robSize)(i => valid(i)(1) := 0.U) // set valid to 0
    List.tabulate(NRReg)(i => rmtValid(i) := false.B) // flush rmt
  }

}
