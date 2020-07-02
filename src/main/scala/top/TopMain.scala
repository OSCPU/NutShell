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
  
  Settings.settings = (board, core) match {
    case ("sim", "seq")    => (CommonSetting.common ++ CoreRelatedSetting.seqCore ++ BoardRelatedSetting.pynq)
    case ("sim", "ooo")    => (CommonSetting.common ++ CoreRelatedSetting.oooCore ++ BoardRelatedSetting.pynq)
    case ("pynq", "seq")   => (CommonSetting.common ++ CoreRelatedSetting.seqCore ++ BoardRelatedSetting.pynq)
    case ("pynq", "ooo")   => (CommonSetting.common ++ CoreRelatedSetting.oooCore ++ BoardRelatedSetting.pynq)
    case ("axu3cg", "seq") => (CommonSetting.common ++ CoreRelatedSetting.seqCore ++ BoardRelatedSetting.axu3cg)
    case ("axu3cg", "ooo") => (CommonSetting.common ++ CoreRelatedSetting.oooCore ++ BoardRelatedSetting.axu3cg)
  }

  if (board == "sim") {
    Driver.execute(args, () => new NOOPSimTop)
  } else {
    Driver.execute(args, () => new Top)
  }
}
