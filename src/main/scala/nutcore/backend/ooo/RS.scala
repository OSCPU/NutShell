// Reservation Station for Out Of Order Execution Backend

package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

trait HasRSConst{
  // val rsSize = 4
  val rsCommitWidth = 2
}

// Reservation Station
class RS(size: Int = 2, pipelined: Boolean = true, fifo: Boolean = false, priority: Boolean = false, checkpoint: Boolean = false, storeBarrier: Boolean = false, storeSeq: Boolean = false, name: String = "unnamedRS") extends NutCoreModule with HasRSConst with HasBackendConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new RenamedDecodeIO))
    val out = Decoupled(new RenamedDecodeIO)
    val cdb = Vec(rsCommitWidth, Flipped(Valid(new OOCommitIO)))
    val mispredictRec = Flipped(new MisPredictionRecIO)
    val flush = Input(Bool())
    val empty = Output(Bool())
    val updateCheckpoint = if (checkpoint) Some(Output(Valid(UInt(log2Up(size).W)))) else None
    val recoverCheckpoint = if (checkpoint) Some(Output(Valid(UInt(log2Up(size).W)))) else None
    val freeCheckpoint = if (checkpoint) Some(Input(Valid(UInt(log2Up(size).W)))) else None
    val stMaskOut = if (storeSeq) Some(Output(UInt(robSize.W))) else None
    val commit = if (!pipelined) Some(Input(Bool())) else None

    val wakeup = Vec(WakeupBusWidth, Flipped(Valid(new WakeupBus)))
    val select = Valid(new WakeupBus)
  })

  //                   Argo's Reservation Station
  //
  //                   RS Entry         Selected Reg
  //              -----------------    ------------         
  //    ENQ   ->  | 0 | 1 | 2 | 3 | -> | selected |  ->  DEQ
  //              -----------------    ------------         
  // allocate     wake up / select        issue
      
  //An inst needs at least 3 cycles to leave RS:
  //cycle 0: inst enters RS
  //cycle 1: generate dequeueSelect
  //cycle 2: selected inst is issued

  val rsSize = size
  val decode  = Mem(rsSize, new RenamedDecodeIO) // TODO: decouple DataSrcIO from DecodeIO
  // val decode  = Reg(Vec(rsSize, new RenamedDecodeIO)) // TODO: decouple DataSrcIO from DecodeIO
  val valid   = RegInit(VecInit(Seq.fill(rsSize)(false.B)))
  val src1Rdy = RegInit(VecInit(Seq.fill(rsSize)(false.B)))
  val src2Rdy = RegInit(VecInit(Seq.fill(rsSize)(false.B)))
  val src1Spec = RegInit(VecInit(Seq.fill(rsSize)(false.B)))
  val src2Spec = RegInit(VecInit(Seq.fill(rsSize)(false.B)))
  val src1SpecSrc = Reg(Vec(rsSize, UInt(log2Up(WakeupBusWidth).W)))
  val src2SpecSrc = Reg(Vec(rsSize, UInt(log2Up(WakeupBusWidth).W)))
  val brMask  = RegInit(VecInit(Seq.fill(rsSize)(0.U(checkpointSize.W))))
  val prfSrc1 = Reg(Vec(rsSize, UInt(prfAddrWidth.W)))
  val prfSrc2 = Reg(Vec(rsSize, UInt(prfAddrWidth.W)))
  val src1    = Reg(Vec(rsSize, UInt(XLEN.W)))
  val src2    = Reg(Vec(rsSize, UInt(XLEN.W)))
  val selected = RegInit(VecInit(Seq.fill(rsSize)(false.B)))

  val validNext = WireInit(valid)
  val instRdy = WireInit(VecInit(List.tabulate(rsSize)(i => 
    (src1Rdy(i) || src1Spec(i)) && 
    (src2Rdy(i) || src2Spec(i)) && 
    valid(i) && !selected(i)
  )))
  val rsEmpty = !valid.asUInt.orR
  val rsFull = valid.asUInt.andR
  val rsAllowin = !rsFull

  val priorityMask = RegInit(VecInit(Seq.fill(rsSize)(VecInit(Seq.fill(rsSize)(false.B)))))
  val forceDequeue = WireInit(false.B)

  def needMispredictionRecovery(brMask: UInt) = {
    io.mispredictRec.valid && io.mispredictRec.redirect.valid && brMask(io.mispredictRec.checkpoint)
  }

  def updateBrMask(brMask: UInt) = {
    brMask & ~ (UIntToOH(io.mispredictRec.checkpoint) & Fill(checkpointSize, io.mispredictRec.valid))
  }

  // Listen to Common Data Bus
  // Here we listen to commit signal chosen by ROB
  // If prf === src, mark it as `ready`

  List.tabulate(rsSize)(i => 
    when(valid(i)){
      List.tabulate(rsCommitWidth)(j =>
        when(!src1Rdy(i) && prfSrc1(i) === io.cdb(j).bits.prfidx && io.cdb(j).valid){
            src1Rdy(i) := true.B
            src1(i) := io.cdb(j).bits.commits
        }
      )
      List.tabulate(rsCommitWidth)(j =>
        when(!src2Rdy(i) && prfSrc2(i) === io.cdb(j).bits.prfidx && io.cdb(j).valid){
            src2Rdy(i) := true.B
            src2(i) := io.cdb(j).bits.commits
        }
      )
      // Update RS according to brMask
      // If a branch inst is canceled by BRU, the following insts with subject to this branch should also be canceled
      brMask(i) := updateBrMask(brMask(i))
      when(needMispredictionRecovery(brMask(i))){ valid(i):= false.B }
    }
  )

  // Listen to Wakeup Bus (Select)
  // If prf === src, mark it as `specwakeup`
  List.tabulate(rsSize)(i => 
    when(valid(i)){
      List.tabulate(WakeupBusWidth)(j =>
        when(!src1Rdy(i) && prfSrc1(i) === io.wakeup(j).bits.pdest && io.wakeup(j).valid){
            src1Spec(i) := true.B
            src1SpecSrc(i) := j.U
        }
      )
      List.tabulate(WakeupBusWidth)(j =>
        when(!src2Rdy(i) && prfSrc2(i) === io.wakeup(j).bits.pdest && io.wakeup(j).valid){
            src2Spec(i) := true.B
            src2SpecSrc(i) := j.U
        }
      )
    }
  )

  // set `specwakeup` flag for enqueue insts
  val enqSrc1Spec = WireInit(false.B)
  val enqSrc2Spec = WireInit(false.B)
  val enqSrc1SpecSrc = Wire(UInt(log2Up(WakeupBusWidth).W))
  val enqSrc2SpecSrc = Wire(UInt(log2Up(WakeupBusWidth).W))
  enqSrc1SpecSrc := DontCare
  enqSrc2SpecSrc := DontCare
  List.tabulate(WakeupBusWidth)(j =>
    when(io.in.bits.prfSrc1 === io.wakeup(j).bits.pdest && io.wakeup(j).valid){
        enqSrc1Spec := true.B
        enqSrc1SpecSrc := j.U
    }
  )
  List.tabulate(WakeupBusWidth)(j =>
    when(io.in.bits.prfSrc2 === io.wakeup(j).bits.pdest && io.wakeup(j).valid){
        enqSrc2Spec := true.B
        enqSrc2SpecSrc := j.U
    }
  )

  // RS enqueue
  io.in.ready := rsAllowin
  io.empty := rsEmpty
  val emptySlot = ~valid.asUInt
  val enqueueSelect = PriorityEncoder(emptySlot) // TODO: replace PriorityEncoder with other logic

  when(io.in.fire()){
    decode(enqueueSelect) := io.in.bits
    valid(enqueueSelect) := true.B
    prfSrc1(enqueueSelect) := io.in.bits.prfSrc1
    prfSrc2(enqueueSelect) := io.in.bits.prfSrc2
    src1Rdy(enqueueSelect) := io.in.bits.src1Rdy
    src2Rdy(enqueueSelect) := io.in.bits.src2Rdy
    src1Spec(enqueueSelect) := enqSrc1Spec
    src2Spec(enqueueSelect) := enqSrc2Spec
    src1SpecSrc(enqueueSelect) := enqSrc1SpecSrc
    src2SpecSrc(enqueueSelect) := enqSrc2SpecSrc
    src1(enqueueSelect) := io.in.bits.decode.data.src1
    src2(enqueueSelect) := io.in.bits.decode.data.src2
    brMask(enqueueSelect) := io.in.bits.brMask
    selected(enqueueSelect) := false.B
  }

  // RS dequeue
  val dequeueSelect = Wire(UInt(log2Up(size).W))
  val dequeueSelectReg = Reg(UInt(log2Up(rsSize).W))
  dequeueSelect := PriorityEncoder(instRdy)
  when(io.out.fire() || forceDequeue){
    if(!checkpoint){
      valid(dequeueSelectReg) := false.B
    }
  }

  val wakeupReady = Wire(Bool())
  wakeupReady := instRdy.reduce(_||_) && !needMispredictionRecovery(brMask(dequeueSelect))
  val rsReadygo = RegInit(false.B) // i.e. selectedValid
  val selectedValid = rsReadygo
  val selectedBrMask = Reg(UInt(robInstCapacity.W)) // FIXME
  val selectedDecode = Reg(new RenamedDecodeIO)

  selectedBrMask := updateBrMask(selectedBrMask) // update brMask if a selected inst is not issued

  when(io.out.fire() || !rsReadygo){
    when(!rsReadygo || io.out.fire()){
      rsReadygo := wakeupReady
      when(wakeupReady){
        dequeueSelectReg := dequeueSelect
        selected(dequeueSelect) := true.B
        selectedBrMask := updateBrMask(brMask(dequeueSelect))
        selectedDecode := decode(dequeueSelect)
        selectedDecode.decode.data.src1 := 
          Mux(src1Spec(dequeueSelect) && !src1Rdy(dequeueSelect), 
            io.wakeup(src1SpecSrc(dequeueSelect)).bits.data,
            src1(dequeueSelect)
          )
        selectedDecode.decode.data.src2 := 
          Mux(src2Spec(dequeueSelect) && !src2Rdy(dequeueSelect), 
            io.wakeup(src2SpecSrc(dequeueSelect)).bits.data,
            src2(dequeueSelect)
          )
      }
    }
  }
  when(io.out.fire()){
    // selected(dequeueSelectReg) := false.B
  } 

  // select bus
  io.select.valid := (!rsReadygo || io.out.fire()) && wakeupReady
  io.select.bits.pdest := decode(dequeueSelect).prfDest
  io.select.bits.data := DontCare

  //When a selected inst is judged to be misprediction:
  //* If that selected inst will stay in RS, invalidate it.
  //* If that inst will be issued this cycle, do nothing.
  when(needMispredictionRecovery(selectedBrMask) && rsReadygo && !io.out.fire() || io.flush){ rsReadygo := false.B }

  io.out.valid := rsReadygo
  io.out.bits := selectedDecode
  io.out.bits.brMask := selectedBrMask

  when(io.flush){
    List.tabulate(rsSize)(i => {
      valid(i) := false.B
      selected(i) := false.B
    })
  }

  Debug(){
    when(io.out.fire()){printf("[ISSUE-"+ name + "] " + "TIMER: %d pc = 0x%x inst %x wen %x id %d\n", GTimer(), io.out.bits.decode.cf.pc, io.out.bits.decode.cf.instr, io.out.bits.decode.ctrl.rfWen, io.out.bits.prfDest)}
  }

  Debug(){
    printf("[RS " + name + "] time %d\n", GTimer())
    printf("[RS " + name + "] pc           v s src1               src2\n")
    for(i <- 0 to (size -1)){
      printf("[RS " + name + "] 0x%x %x %x %x %x %x %x %d", decode(i).decode.cf.pc, valid(i), selected(i), src1Rdy(i), src1(i), src2Rdy(i), src2(i), decode(i).prfDest)
      printf(" mask %x", brMask(i))
      when(valid(i)){printf(" valid")}
      when(rsReadygo && i.U === dequeueSelectReg){printf(" selected")}
      when(i.U === dequeueSelectReg && io.out.fire()){printf(" issue")}
      printf("\n")
      // printf("\n")
    }
    printf("[RS " + name + "] selected: valid %x pc %x inst %x src1 %x src2 %x\n", selectedValid, selectedDecode.decode.cf.pc, selectedDecode.decode.cf.instr, selectedDecode.decode.data.src1, selectedDecode.decode.data.src2)
    printf("[RS " + name + "] wakeup0: valid %x pdst %d data %x\n", io.wakeup(0).valid, io.wakeup(0).bits.pdest, io.wakeup(0).bits.data)
    printf("[RS " + name + "] wakeup1: valid %x pdst %d data %x\n", io.wakeup(1).valid, io.wakeup(1).bits.pdest, io.wakeup(1).bits.data)
  }

  // fix unpipelined 
  // when `pipelined` === false, RS helps unpipelined FU to store its uop for commit
  // if an unpipelined fu can store uop itself, set `pipelined` to true (it behaves just like a pipelined FU)
  if(!pipelined){
    val fuValidReg = RegInit(false.B)
    val brMaskPReg = RegInit(0.U(checkpointSize.W))
    val fuFlushReg = RegInit(false.B)
    val fuDecodeReg = RegEnable(io.out.bits, io.out.fire())
    brMaskPReg := updateBrMask(brMaskPReg)
    when(io.out.fire()){ 
      fuValidReg := true.B 
      brMaskPReg := updateBrMask(brMask(dequeueSelectReg))
    }
    when(io.commit.get){ fuValidReg := false.B }
    when((io.flush || needMispredictionRecovery(brMaskPReg)) && fuValidReg || (io.flush || needMispredictionRecovery(brMask(dequeueSelectReg))) && io.out.fire()){ fuFlushReg := true.B }
    when(io.commit.get){ fuFlushReg := false.B }
    when(fuValidReg){ io.out.bits := fuDecodeReg }
    when(fuValidReg){ io.out.valid := true.B && !fuFlushReg}
    io.out.bits.brMask := brMaskPReg
    Debug(){
      printf("[RS " + name + "] pc 0x%x valid %x flush %x brMaskPReg %x prfidx %d   in %x\n", fuDecodeReg.decode.cf.pc, fuValidReg, fuFlushReg, brMaskPReg, fuDecodeReg.prfDest, io.out.fire())
    }
  }

  if(fifo){
    require(!priority)
    require(!storeBarrier)
    dequeueSelect := OHToUInt(List.tabulate(rsSize)(i => {
      !priorityMask(i).asUInt.orR && valid(i) && !selected(i)
    }))
    wakeupReady := src1Rdy(dequeueSelect) && src2Rdy(dequeueSelect) && valid(dequeueSelect)
    io.out.valid := rsReadygo && valid(dequeueSelectReg)
    // update priorityMask
    List.tabulate(rsSize)(i => 
      when(needMispredictionRecovery(brMask(i))){ 
        List.tabulate(rsSize)(j => 
          priorityMask(j)(i):= false.B 
        )
      }
    )
    when(io.in.fire()){priorityMask(enqueueSelect) := valid}
    when(io.out.fire()){(0 until rsSize).map(i => priorityMask(i)(dequeueSelectReg) := false.B)}
    when(io.flush){(0 until rsSize).map(i => priorityMask(i) := VecInit(Seq.fill(rsSize)(false.B)))}
  }

  if(priority){
    require(!fifo)
    require(!storeBarrier)
    dequeueSelect := OHToUInt(List.tabulate(rsSize)(i => {
      !(priorityMask(i).asUInt & instRdy.asUInt).orR & instRdy(i)
    }))
    // update priorityMask
    List.tabulate(rsSize)(i => 
      when(needMispredictionRecovery(brMask(i))){ 
        List.tabulate(rsSize)(j => 
          priorityMask(j)(i):= false.B 
        )
      }
    )
    when(io.in.fire()){ priorityMask(enqueueSelect) := valid}
    when(io.out.fire()){(0 until rsSize).map(i => priorityMask(i)(dequeueSelectReg) := false.B)}
    when(io.flush){(0 until rsSize).map(i => priorityMask(i) := VecInit(Seq.fill(rsSize)(false.B)))}
  }

  if(storeBarrier){
    require(!priority)
    require(!fifo)
    val needStore = Reg(Vec(rsSize, Bool()))
    val dequeueSelectVec = VecInit(List.tabulate(rsSize)(i => {
      valid(i) &&
      Mux(
        needStore(i), 
        !(priorityMask(i).asUInt.orR), // there is no other inst ahead
        !((priorityMask(i).asUInt & needStore.asUInt).orR) // there is no store (with no valid addr) ahead
      ) &&
      !(priorityMask(i).asUInt & instRdy.asUInt).orR & instRdy(i)
    }))
    dequeueSelect := OHToUInt(dequeueSelectVec)
    wakeupReady := instRdy(dequeueSelect) && dequeueSelectVec(dequeueSelect)
    // update priorityMask
    List.tabulate(rsSize)(i => 
      when(needMispredictionRecovery(brMask(i))){ 
        List.tabulate(rsSize)(j => 
          priorityMask(j)(i):= false.B 
        )
      }
    )
    when(io.in.fire()){
      priorityMask(enqueueSelect) := valid
      needStore(enqueueSelect) := LSUOpType.needMemWrite(io.in.bits.decode.ctrl.fuOpType)
    }
    when(io.out.fire()){(0 until rsSize).map(i => priorityMask(i)(dequeueSelectReg) := false.B)}
    when(io.flush){(0 until rsSize).map(i => priorityMask(i) := VecInit(Seq.fill(rsSize)(false.B)))}
  }

  if(storeSeq){
    require(!priority || !fifo || !storeBarrier)
    val stMask = Reg(Vec(rsSize, Vec(robSize, Bool())))
    // val stMask = RegInit(VecInit(Seq.fill(rsSize)(VecInit(Seq.fill(robSize)(false.B)))))
    val needStore = Reg(Vec(rsSize, Bool()))
    val stMaskReg = RegInit(VecInit(Seq.fill(robSize)(false.B)))

    if(EnableOutOfOrderMemAccess){
      val dequeueSelectVec = VecInit(List.tabulate(rsSize)(i => {
        valid(i) &&
        Mux(
          needStore(i), 
          !(priorityMask(i).asUInt.orR), // there is no other inst ahead
          true.B
        ) &&
        !(priorityMask(i).asUInt & instRdy.asUInt).orR & instRdy(i)
      }))
      dequeueSelect := OHToUInt(dequeueSelectVec)
      wakeupReady := instRdy(dequeueSelect) && dequeueSelectVec(dequeueSelect) && !needMispredictionRecovery(brMask(dequeueSelect))
    }else{
      dequeueSelect := OHToUInt(List.tabulate(rsSize)(i => {
        !priorityMask(i).asUInt.orR && valid(i)
      }))
      wakeupReady := instRdy(dequeueSelect) && !priorityMask(dequeueSelect).asUInt.orR && valid(dequeueSelect) && !needMispredictionRecovery(brMask(dequeueSelect))
    }
  
    // update priorityMask
    List.tabulate(rsSize)(i => 
      when(needMispredictionRecovery(brMask(i))){ 
        List.tabulate(rsSize)(j => 
          priorityMask(j)(i):= false.B 
        )
      }
    )
    when(io.in.fire()){
      priorityMask(enqueueSelect) := valid
      needStore(enqueueSelect) := LSUOpType.needMemWrite(io.in.bits.decode.ctrl.fuOpType)
    }
    when(io.out.fire()){(0 until rsSize).map(i => priorityMask(i)(dequeueSelectReg) := false.B)}
    when(io.flush){(0 until rsSize).map(i => priorityMask(i) := VecInit(Seq.fill(rsSize)(false.B)))}

    //update stMask
    val robStoreInstVec = WireInit(0.U(robSize.W))
    BoringUtils.addSink(robStoreInstVec, "ROBStoreInstVec")
    (0 until robSize).map(i => {
      when(!robStoreInstVec(i)){stMaskReg(i) := false.B}
    })
    when(io.in.fire() && LSUOpType.needMemWrite(io.in.bits.decode.ctrl.fuOpType)){
      stMaskReg(io.in.bits.prfDest(prfAddrWidth-1,1)) := true.B
    }
    when(io.in.fire()){
      stMask(enqueueSelect) := stMaskReg
    }
    when(io.out.fire()){
      stMaskReg(io.out.bits.prfDest(prfAddrWidth-1,1)) := false.B
      (0 until rsSize).map(i => {
        stMask(i)(io.out.bits.prfDest(prfAddrWidth-1,1)) := false.B
      })
    }
    when(io.flush){(0 until robSize).map(i => {
      stMaskReg(i) := false.B
    })}
    io.stMaskOut.get := stMask(dequeueSelectReg).asUInt
  }

  if(checkpoint){
    require(priority)

    io.updateCheckpoint.get.valid := io.in.fire()
    io.updateCheckpoint.get.bits := enqueueSelect
    io.recoverCheckpoint.get.valid := DontCare
    io.recoverCheckpoint.get.bits := dequeueSelectReg

    val pending = RegInit(VecInit(Seq.fill(rsSize)(false.B)))
    when(io.in.fire()){pending(enqueueSelect) := true.B}
    when(io.out.fire()){pending(dequeueSelectReg) := false.B}
    when(io.flush){List.tabulate(rsSize)(i => pending(i) := false.B)}
    instRdy := VecInit(List.tabulate(rsSize)(i => src1Rdy(i) && src2Rdy(i) && valid(i) && pending(i) && !selected(i)))
    when(io.freeCheckpoint.get.valid){valid(io.freeCheckpoint.get.bits) := false.B}

    List.tabulate(rsSize)(i => 
      when(needMispredictionRecovery(brMask(i))){ 
        List.tabulate(rsSize)(j => 
          priorityMask(j)(i):= false.B 
        )
      }
    )
    when(io.in.fire()){priorityMask(enqueueSelect) := (valid.asUInt & pending.asUInt).asBools}
    when(io.out.fire()){(0 until rsSize).map(i => priorityMask(i)(dequeueSelectReg) := false.B)}
    when(io.flush){(0 until rsSize).map(i => priorityMask(i) := VecInit(Seq.fill(rsSize)(false.B)))}

    BoringUtils.addSource(PopCount(valid) === 0.U, "perfCntCondMbrInROB_0")
    BoringUtils.addSource(PopCount(valid) === 1.U, "perfCntCondMbrInROB_1")
    BoringUtils.addSource(PopCount(valid) === 2.U, "perfCntCondMbrInROB_2")
    BoringUtils.addSource(PopCount(valid) === 3.U, "perfCntCondMbrInROB_3")
    BoringUtils.addSource(PopCount(valid) >   3.U, "perfCntCondMbrInROB_4")
  }

}