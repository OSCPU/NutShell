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
import difftest.common.LogPerfControl
import nutcore.NutCoreConfig
import utils.LogLevel.LogLevel

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

  def control(): (Bool, UInt) = {
    val control = LogPerfControl()
    (control.logEnable, control.timer)
  }

  def apply(debugLevel: LogLevel)
           (prefix: Boolean, cond: Bool, pable: Printable)
           (implicit name: String): Any = {
    if (NutCoreConfig().EnableDebug){
      val c = control()
      val commonInfo = p"[${c._2}] $name: "
      when (cond && c._1) {
        if(prefix) printf(commonInfo)
        printf(pable)
      }
    } 
  }
}

sealed abstract class LogHelper(val logLevel: LogLevel) {

  def apply(cond: Bool, fmt: String, data: Bits*)(implicit name: String): Any =
    apply(cond, Printable.pack(fmt, data:_*))
  def apply(cond: Bool, pable: Printable)(implicit name: String): Any = apply(true, cond, pable)
  def apply(fmt: String, data: Bits*)(implicit name: String): Any =
    apply(true.B, Printable.pack(fmt, data:_*))
  def apply(pable: Printable)(implicit name: String): Any = apply(true.B, pable)
  def apply(prefix: Boolean, fmt: String, data: Bits*)(implicit name: String): Any = apply(prefix, true.B, Printable.pack(fmt, data:_*))
  def apply(prefix: Boolean, pable: Printable)(implicit name: String): Any = apply(prefix, true.B, pable)
  def apply(prefix: Boolean, cond: Bool, fmt: String, data: Bits*)(implicit name: String): Any =
    apply(prefix, cond, Printable.pack(fmt, data:_*))
  def apply(prefix: Boolean, cond: Bool, pable: Printable)(implicit name: String): Any =
    LogUtil(logLevel)(prefix, cond, pable)

  // NOOP/NutShell style debug
  def apply(flag: Boolean = NutCoreConfig().EnableDebug, cond: Bool = true.B)(body: => Unit): Any = {
    if(NutCoreConfig().EnhancedLog){
      if(flag) { when (cond && LogUtil.control()._1) { body } }
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
