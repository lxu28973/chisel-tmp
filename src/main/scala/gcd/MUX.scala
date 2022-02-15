// See README.md for license details.

package gcd

import chisel3._
import chisel3.util.{Mux1H, MuxCase, MuxLookup, UIntToOH, log2Ceil}
import agile.config._

import scala.math.BigInt.int2bigInt

case object SelNum extends Field[Int](32)
case object ToOneHotFirst extends Field[Boolean](false)
case object RegIn extends Field[Boolean](false)
case object RegOut extends Field[Boolean](false)

class MUXConfig(w: Int, n: Int, oh: Boolean) extends Config(
  (site, here, tail) => {
    case WordWidth => w
    case SelNum => n
    case ToOneHotFirst => oh
    case RegOut => true
  }
)

class MUX(implicit p:Parameters) extends Module {
  val num = p(SelNum)
  val width = p(WordWidth)
  val useOH = p(ToOneHotFirst)
  val regIn = p(RegIn)
  val regOut = p(RegOut)
  override val desiredName = s"MUX$num" + (if(useOH) "_OH" else "")

  val io = IO(new Bundle {
    val in  = Input(Vec(num, UInt(width.W)))
    val sel = Input(UInt(log2Ceil(num).W))
    val out = Output(UInt(width.W))
  })

  val in = if (regIn) RegNext(io.in) else io.in
  val sel = if (regIn) RegNext(io.sel) else io.sel
  val out = if (regOut) RegInit(0.U(width.W)) else WireDefault(0.U(width.W))

  if (useOH) {
    val sel_oh = UIntToOH(sel, num)
    val muxPair = sel_oh.asBools zip in
    out := Mux1H(muxPair)
  }
  else {
    out := MuxLookup(sel, 0.U(width.W),
      (0 until num).map(_.U) zip in
    )
//    out := MuxCase(0.U, (0 to 15).map(_.U === sel) zip in)
//    out := Mux(sel === 0.U, in(0), Mux(sel === 1.U, in(1), Mux(sel === 2.U, in(2), Mux(sel === 3.U, in(3), Mux(sel === 4.U, in(4), Mux(sel === 5.U, in(5), Mux(sel === 6.U, in(6), Mux(sel === 7.U, in(7), Mux(sel === 8.U, in(8), Mux(sel === 9.U, in(9), Mux(sel === 10.U, in(10), Mux(sel === 11.U, in(11), Mux(sel === 12.U, in(12), Mux(sel === 13.U, in(13), Mux(sel === 14.U, in(14), in(15))))))))))))))))
  }

  io.out := out

}

object MUXGen extends App {
  import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

  // use "--help" to see more options
  val chiselArgs = Array("-X", "verilog", "-td", "verilog_gen_dir",
    "--emission-options", "disableMemRandomization,disableRegisterRandomization")

  for(i <- 4 to 4) {
    for (j <- Seq(true, false))
    (new chisel3.stage.ChiselStage).execute(
      chiselArgs, Seq(ChiselGeneratorAnnotation(() => new MUX()(new MUXConfig(1, 2.pow(i).toInt, j)))))
  }
}
