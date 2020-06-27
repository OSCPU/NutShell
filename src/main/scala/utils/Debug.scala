package utils

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils.LogLevel.LogLevel

import noop.NOOPConfig

object LogLevel extends Enumeration {
  type LogLevel = Value

  val ALL   = Value(0, "ALL  ")
  val DEBUG = Value("DEBUG")
  val INFO  = Value("INFO ")
  val WARN  = Value("WARN ")
  val ERROR = Value("ERROR")
  val OFF   = Value("OFF  ")
}

object LogUtil {

  def displayLog: Bool = {
    val disp_begin, disp_end = WireInit(0.U(64.W))
    BoringUtils.addSink(disp_begin, "DISPLAY_LOG_START")
    BoringUtils.addSink(disp_end, "DISPLAY_LOG_END")
    assert(disp_begin <= disp_end)
    (GTimer() >= disp_begin) && (GTimer() <= disp_end)
  }

  def LogLevel: UInt = {
    val log_level = WireInit(0.U(64.W))
    BoringUtils.addSink(log_level, "DISPLAY_LOG_LEVEL")
    log_level
  }

  def apply(debugLevel: LogLevel)
           (cond: Bool, pable: Printable)
           (implicit m: Module): Any = {
    val commonInfo = p"[$debugLevel][time=${GTimer()}] ${m.name}: "
    when (debugLevel.id.U >= LogLevel && cond && displayLog) {
      printf(commonInfo + pable)
    }
  }
}

sealed abstract class LogHelper(val logLevel: LogLevel) {

  def apply(cond: Bool, fmt: String, data: Bits*)(implicit m: Module): Any =
    apply(cond, Printable.pack(fmt, data:_*))
  def apply(cond: Bool, pable: Printable)(implicit m: Module): Any = LogUtil(logLevel)(cond, pable)
  def apply(fmt: String, data: Bits*)(implicit m: Module): Any =
    apply(true.B, Printable.pack(fmt, data:_*))
  def apply(pable: Printable)(implicit m: Module): Any = LogUtil(logLevel)(true.B, pable)

  // NOOP/NutShell style debug
  def apply(flag: Boolean = NOOPConfig().EnableDebug, cond: Bool = true.B)(body: => Unit): Any =
    if(flag) { when (logLevel.id.U >= LogUtil.LogLevel && cond && LogUtil.displayLog) { body } }
}

object Debug extends LogHelper(LogLevel.DEBUG)
object Info extends LogHelper(LogLevel.INFO)
object Warn extends LogHelper(LogLevel.WARN)
object Error extends LogHelper(LogLevel.ERROR)

object ShowType {
  def apply[T: Manifest](t: T) = println(manifest[T])
}
