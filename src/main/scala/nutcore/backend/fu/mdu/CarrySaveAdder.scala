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

package nutcore

import chisel3._
import chisel3.util._

object CSA {
  def checkInput(len: Int, width: Int, in: List[UInt]) = {
    require(in.length == width)
    in.map(x => require(x.getWidth == len))
  }

  def CSA2_2(len: Int)(in: List[UInt]): List[UInt] = {
    checkInput(len, 2, in);
    val temp = Wire(Vec(len, UInt(2.W)))
    for((t, i) <- temp.zipWithIndex){
      val (a, b) = (in(0)(i), in(1)(i))
      val sum = a ^ b
      val cout = a & b
      t := Cat(cout, sum)
    }
    val out = Wire(Vec(2, UInt(len.W)))
    out.zipWithIndex.foreach({case(x, i) => x := Cat(temp.reverse map(_(i)))})
    out.toList
  }

  def CSA3_2(len: Int)(in: List[UInt]): List[UInt] = {
    checkInput(len, 3, in);
    val temp = Wire(Vec(len, UInt(2.W)))
    for((t, i) <- temp.zipWithIndex){
      val (a, b, cin) = (in(0)(i), in(1)(i), in(2)(i))
      val a_xor_b = a ^ b
      val a_and_b = a & b
      val sum = a_xor_b ^ cin
      val cout = a_and_b | (a_xor_b & cin)
      t := Cat(cout, sum)
    }
    val out = Wire(Vec(2, UInt(len.W)))
    out.zipWithIndex.foreach({case(x, i) => x := Cat(temp.reverse map(_(i)))})
    out.toList
  }

  def CSA5_3(len: Int)(in: List[UInt]): List[UInt] = {
    checkInput(len, 5, in);
    val FA0_out = CSA3_2(len)(in.take(3))
    val FA1_out = CSA3_2(len)(List(FA0_out(0), in(3), in(4)))
    List(FA1_out(0), FA0_out(1), FA1_out(1))
  }

  def C22 = CSA2_2(1)(_)
  def C32 = CSA3_2(1)(_)
  def C53 = CSA5_3(1)(_)
}
