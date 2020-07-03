package top

import nutshell.NutShellConfig
import system.NutShellSoC
import device.{AXI4Timer, AXI4VGA, AXI4Flash}
import gpu._
import sim.NutShellSimTop

import chisel3._

class Top extends Module {
  val io = IO(new Bundle{})
  val nutshell = Module(new NutShellSoC()(NutShellConfig()))
  val timer = Module(new AXI4Timer)
  val vga = Module(new AXI4VGA)
  val flash = Module(new AXI4Flash)
//  val gpu = Module(new AXI4GPU)

  nutshell.io := DontCare
  timer.io := DontCare
  vga.io := DontCare
  flash.io := DontCare
//  gpu.io := DontCare
  dontTouch(nutshell.io)
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
  
  val s = (board match {
    case "sim"    => Nil
    case "pynq"   => PynqSettings()
    case "axu3cg" => Axu3cgSettings()
    case "PXIe"   => PXIeSettings()
  } ) ++ ( core match {
    case "seq"  => InOrderSettings()
    case "ooo"  => OOOSettings()
    case "small"=> EmbededSettings()
  } )
  s.map{Settings.settings += _} // add and overwrite DefaultSettings
  println("====== Settings = (" + board + ", " +  core + ") ======")
  Settings.settings.toList.sortBy(_._1)(Ordering.String).map(s => println(s._1 + " = " + s._2))

  if (board == "sim") {
    Driver.execute(args, () => new NutShellSimTop)
  } else {
    Driver.execute(args, () => new Top)
  }
}
