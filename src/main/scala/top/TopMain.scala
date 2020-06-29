package top

import noop.NOOPConfig
import system.NOOPSoC
import device.{AXI4Timer, AXI4VGA, AXI4Flash}
import gpu._

import chisel3._

class Top extends Module {
  val io = IO(new Bundle{})
  val noop = Module(new NOOPSoC()(NOOPConfig()))
  val timer = Module(new AXI4Timer)
  val vga = Module(new AXI4VGA)
  val flash = Module(new AXI4Flash)
//  val gpu = Module(new AXI4GPU)

  noop.io := DontCare
  timer.io := DontCare
  vga.io := DontCare
  flash.io := DontCare
//  gpu.io := DontCare
  dontTouch(noop.io)
  dontTouch(timer.io)
  dontTouch(vga.io)
  dontTouch(flash.io)
//  dontTouch(gpu.io)
}

object TopMain extends App {
  def parseArgs(info: String, args: Array[String]): String = {
    var target = ""
    for (arg <- args) { if (arg.startsWith(info + "=") == true) { target = arg } }
    require(target != "")
    target.substring(info.length()+1)
  }
  val board = parseArgs("BOARD", args)
  val core = parseArgs("CORE", args)
  
  val (bootmap, valuemap) = (board, core) match {
    case ("sim", "seq")    => (CommonSetting.commonBoolMap ++ CoreRelatedSetting.seqCoreBoolMap, BoardRelatedSetting.pynqValueMap)
    case ("sim", "ooo")    => (CommonSetting.commonBoolMap ++ CoreRelatedSetting.oooCoreBoolMap, BoardRelatedSetting.pynqValueMap)
    case ("pynq", "seq")   => (CommonSetting.commonBoolMap ++ CoreRelatedSetting.seqCoreBoolMap, BoardRelatedSetting.pynqValueMap)
    case ("pynq", "ooo")   => (CommonSetting.commonBoolMap ++ CoreRelatedSetting.oooCoreBoolMap, BoardRelatedSetting.pynqValueMap)
    case ("axu3cg", "seq") => (CommonSetting.commonBoolMap ++ CoreRelatedSetting.seqCoreBoolMap, BoardRelatedSetting.axu3cgValueMap)
    case ("axu3cg", "ooo") => (CommonSetting.commonBoolMap ++ CoreRelatedSetting.oooCoreBoolMap, BoardRelatedSetting.axu3cgValueMap)
  }

  Settings.boolMap  = bootmap
  Settings.valueMap = valuemap
  
  if (board == "sim") {
    Driver.execute(args, () => new NOOPSimTop)
  } else {
    Driver.execute(args, () => new Top)
  }
}
