package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

trait HasRSConst{
  // val rsSize = 4
  val rsCommitWidth = 2
}

// Reservation Station
class RS(size: Int = 4, pipelined: Boolean = true, fifo: Boolean = false, priority: Boolean = false, name: String = "unnamedRS") extends NOOPModule with HasRSConst with HasBackendConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new RenamedDecodeIO))
    val out = Decoupled(new RenamedDecodeIO)
    val cdb = Vec(rsCommitWidth, Flipped(Valid(new OOCommitIO)))
    val flush = Input(Bool())
    val empty = Output(Bool())
    val commit = if (!pipelined) Some(Input(Bool())) else None
  })

  val rsSize = size
  val decode  = Mem(rsSize, new RenamedDecodeIO) // TODO: decouple DataSrcIO from DecodeIO
  // val decode  = Reg(Vec(rsSize, new RenamedDecodeIO)) // TODO: decouple DataSrcIO from DecodeIO
  val valid   = RegInit(VecInit(Seq.fill(rsSize)(false.B)))
  val src1Rdy = RegInit(VecInit(Seq.fill(rsSize)(false.B)))
  val src2Rdy = RegInit(VecInit(Seq.fill(rsSize)(false.B)))
  // val prfDest = Reg(Vec(rsSize, UInt(prfAddrWidth.W)))
  val prfSrc1 = Reg(Vec(rsSize, UInt(prfAddrWidth.W)))
  val prfSrc2 = Reg(Vec(rsSize, UInt(prfAddrWidth.W)))
  val src1    = Reg(Vec(rsSize, UInt(XLEN.W)))
  val src2    = Reg(Vec(rsSize, UInt(XLEN.W)))
//   val imm = Vec(rsSize, Reg(UInt(XLEN.W)))

  val instRdy = WireInit(VecInit(List.tabulate(rsSize)(i => src1Rdy(i) && src2Rdy(i) && valid(i))))
  val rsEmpty = !valid.asUInt.orR
  val rsFull = valid.asUInt.andR
  val rsAllowin = !rsFull
  val rsReadygo = Wire(Bool())
  rsReadygo := instRdy.foldRight(false.B)((sum, i) => sum|i)

  val forceDequeue = WireInit(false.B)
  // Listen to Common Data Bus
  // Here we listen to commit signal chosen by ROB?
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
    src1(enqueueSelect) := io.in.bits.decode.data.src1
    src2(enqueueSelect) := io.in.bits.decode.data.src2
  }

  // RS dequeue
  io.out.valid := rsReadygo
  val dequeueSelect = Wire(UInt(log2Up(size).W))
  dequeueSelect := PriorityEncoder(instRdy) // TODO: replace PriorityEncoder with other logic
  when(io.out.fire() || forceDequeue){
    valid(dequeueSelect) := false.B
  }

  io.out.bits := decode(dequeueSelect)
  io.out.bits.decode.data.src1 := src1(dequeueSelect)
  io.out.bits.decode.data.src2 := src2(dequeueSelect)

  when(io.flush){
    List.tabulate(rsSize)(i => 
      valid(i) := false.B
    )
  }

  Debug(){
    when(io.out.fire()){printf("[ISSUE-"+ name + "] " + "TIMER: %d pc = 0x%x inst %x wen %x wdst %x\n", GTimer(), io.out.bits.decode.cf.pc, io.out.bits.decode.cf.instr, io.out.bits.decode.ctrl.rfWen, io.out.bits.decode.ctrl.rfDest)}
  }

  Debug(){
    printf("[RS " + name + "] time %d\n", GTimer())
    printf("[RS " + name + "] pc           v src1               src2\n")
    for(i <- 0 to (size -1)){
      printf("[RS " + name + "] 0x%x %x %x %x %x %x", decode(i).decode.cf.pc, valid(i), src1Rdy(i), src1(i), src2Rdy(i), src2(i))
      when(valid(i)){printf("  valid")}
      printf("\n")
    }
  }

  // fix unpipelined 
  // when `pipelined` === false, RS helps unpipelined FU to store its uop for commit
  // if an unpipelined fu can store uop itself, set `pipelined` to true (it behaves just like a pipelined FU)
  if(!pipelined){
    val fuValidReg = RegInit(false.B)
    val fuFlushReg = RegInit(false.B)
    val fuDecodeReg = RegEnable(io.out.bits, io.out.fire())
    when(io.out.fire()){ fuValidReg := true.B }
    when(io.commit.get){ fuValidReg := false.B }
    when(io.flush && (fuValidReg || io.out.fire())){ fuFlushReg := true.B }
    when(io.commit.get){ fuFlushReg := false.B }
    when(fuValidReg){ io.out.bits := fuDecodeReg }
    when(fuValidReg){ io.out.valid := true.B && !fuFlushReg}
    assert(!(io.out.fire() && io.commit.get && fuValidReg && !io.flush))
  }

  if(fifo){
    require(!priority)
    val queue = Module(new FlushableQueue(UInt(log2Up(size).W), size))
    queue.io.flush := io.flush
    queue.io.enq.bits := enqueueSelect
    queue.io.enq.valid := io.in.fire()
    queue.io.deq.ready := io.out.fire()
    dequeueSelect := queue.io.deq.bits
    rsReadygo := instRdy(dequeueSelect)
    io.in.ready := rsAllowin // && queue.enq.ready
    io.out.valid := rsReadygo && queue.io.deq.valid
    assert(!(rsAllowin && !queue.io.enq.ready))
  }

  if(priority){
    require(!fifo)
    val priorityMask = RegInit(VecInit(Seq.fill(rsSize)(VecInit(Seq.fill(rsSize)(false.B)))))
    dequeueSelect := OHToUInt(List.tabulate(rsSize)(i => {
      !(priorityMask(i).asUInt & instRdy.asUInt).orR & instRdy(i)
    }))
    // update priorityMask
    when(io.in.fire()){priorityMask(enqueueSelect) := valid}
    when(io.out.fire()){(0 until rsSize).map(i => priorityMask(i)(dequeueSelect) := false.B)}
    when(io.flush){(0 until rsSize).map(i => priorityMask(i) := VecInit(Seq.fill(rsSize)(false.B)))}
  }

}