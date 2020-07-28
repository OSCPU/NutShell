/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
* 
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2. 
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2 
* 
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER 
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR 
* FIT FOR A PARTICULAR PURPOSE.  
*
* See the Mulan PSL v2 for more details.  
***************************************************************************************/

package utils

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils.LogLevel.LogLevel

import nutcore.NutCoreConfig

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
  def apply(flag: Boolean = NutCoreConfig().EnableDebug, cond: Bool = true.B)(body: => Unit): Any = {
    if(NutCoreConfig().EnhancedLog){
      if(flag) { when (logLevel.id.U >= LogUtil.LogLevel && cond && LogUtil.displayLog) { body } }
    } else {
      if(flag) { when (cond) { body } }
    }
  }
}

object Debug extends LogHelper(LogLevel.DEBUG)
object Info extends LogHelper(LogLevel.INFO)
object Warn extends LogHelper(LogLevel.WARN)
object Error extends LogHelper(LogLevel.ERROR)

object ShowType {
  def apply[T: Manifest](t: T) = println(manifest[T])
}
