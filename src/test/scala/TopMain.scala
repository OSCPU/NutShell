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

package top

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._
import device.AXI4VGA
import difftest.{DifftestModule, DifftestTopIO, HasDiffTestInterfaces}
import nutcore.NutCoreConfig
import sim.NutShellSim
import system.NutShell

class Top extends Module {
  val io = IO(new Bundle{})
  val nutshell = Module(new NutShell()(NutCoreConfig()))
  val vga = Module(new AXI4VGA)

  nutshell.io := DontCare
  vga.io := DontCare
  dontTouch(nutshell.io)
  dontTouch(vga.io)
}

class FpgaDiffTop extends NutShell()(NutCoreConfig(FPGADifftest = true)) with HasDiffTestInterfaces {
  override def desiredName: String = "NutShell"
  override def cpuName: Option[String] = Some("NutShell")
}

object TopMain extends App {
  def parseArgs(info: String, args: Array[String]): String = {
    var target = ""
    for (arg <- args) { if (arg.startsWith(info + "=") == true) { target = arg } }
    require(target != "")
    target.substring(info.length()+1)
  }
  val (newArgs, firtoolOptions) = DifftestModule.parseArgs(args)
  val board = parseArgs("BOARD", newArgs)
  val core = parseArgs("CORE", newArgs)

  val s = (board match {
    case "sim"    => Nil
    case "pynq"   => PynqSettings()
    case "axu3cg" => Axu3cgSettings()
    case "fpgadiff" => FpgaDiffSettings()
    case "PXIe"   => PXIeSettings()
  } ) ++ ( core match {
    case "inorder"  => InOrderSettings()
    case "ooo"  => OOOSettings()
    case "embedded"=> EmbededSettings()
  } )
  s.foreach{Settings.settings += _} // add and overwrite DefaultSettings
  println("====== Settings = (" + board + ", " +  core + ") ======")
  Settings.settings.toList.sortBy(_._1)(Ordering.String).foreach {
    case (f, v: Long) =>
      println(f + " = 0x" + v.toHexString)
    case (f, v) =>
      println(f + " = " + v)
  }

  val generator = if (board == "sim") {
    ChiselGeneratorAnnotation(() => DifftestModule.top(new NutShellSim))
  } else if (board == "fpgadiff") {
    ChiselGeneratorAnnotation(() => DifftestModule.top(new FpgaDiffTop))
  }
  else {
    ChiselGeneratorAnnotation(() => new Top)
  }
  var exe_args = newArgs.filter{
    value => value.forall(char => char!='=')
  }
  (new ChiselStage).execute(newArgs, Seq(generator) ++ firtoolOptions
    :+ CIRCTTargetAnnotation(CIRCTTarget.SystemVerilog)
    :+ FirtoolOption("--disable-annotation-unknown")
    :+ FirtoolOption("--default-layer-specialization=enable")
  )
}