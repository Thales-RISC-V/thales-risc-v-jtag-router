// vim: tabstop=2 expandtab shiftwidth=2 softtabstop=2
package jtag_router
import chisel3._
import chisel3.util._
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.jtag._

class BUFGMUX_CTRL extends BlackBox {
  val io = IO(new Bundle {
    val O = Output(Clock())
    val I0 = Input(Clock())
    val I1 = Input(Clock())
    val S = Input(Bool())
  })
}

object BUFGMUX_CTRL {
  def apply (out: Clock, in0: Clock, in1: Clock, sel: Bool) : Clock = {
    val pad = Module(new BUFGMUX_CTRL())
    pad.io.I0 := in0
    pad.io.I1 := in1
    pad.io.S := sel
    out := pad.io.O
    pad.io.O
  }
}

class JTAGRouter(val cpu_count:Int) extends Module {

  val jtag0_to_all = 0.U
  val jtag0_to_cpu0 = 1.U
  val jtag0_to_cpu1 = 2.U
  val jtag0_to_cpu2 = 3.U

  val io = IO(new Bundle{
    val cpu_jtag = Vec(cpu_count, new JTAGIO(hasTRSTn = false))
    val input_jtag = Vec(2, Flipped(new JTAGIO(hasTRSTn = false)))
    val selector = Input(UInt(2.W))
  })

  val cpu_combinations = io.cpu_jtag.combinations(2).toList

  val jtag_tck = Wire(Vec(cpu_count, Clock()))
  val tck_sel = Wire(Vec(cpu_count, Bool()))

  /* instantiate clock muxes */
  (jtag_tck zip tck_sel).foreach { case(tck, sel) =>
    BUFGMUX_CTRL(tck, io.input_jtag(0).TCK, io.input_jtag(1).TCK, sel)
  }

  /* connect output clocks */
  io.cpu_jtag.zipWithIndex.foreach{ case(j, i) =>
    j.TCK := jtag_tck(i)
  }

  when(io.selector === jtag0_to_cpu0) {

    /* route jtag1 to cpu 1 and 2 */
    cpu_combinations(2).foreach { j =>
      //j.TCK := io.input_jtag(1).TCK
      j.TDI := io.input_jtag(1).TDI
      j.TMS := io.input_jtag(1).TMS
    }

    io.input_jtag(1).TDO.data := cpu_combinations(2)(0).TDO.data | cpu_combinations(2)(1).TDO.data
    io.input_jtag(1).TDO.driven := cpu_combinations(2)(0).TDO.driven | cpu_combinations(2)(1).TDO.driven

    /* mux clocks */
    tck_sel(0) := false.B
    tck_sel(1) := true.B
    tck_sel(2) := true.B

    /* route jtag0 to cpu0 */
    //io.cpu_jtag(0).TCK := io.input_jtag(0).TCK
    io.cpu_jtag(0).TDI := io.input_jtag(0).TDI
    io.cpu_jtag(0).TMS := io.input_jtag(0).TMS
    io.input_jtag(0).TDO.data := io.cpu_jtag(0).TDO.data
    io.input_jtag(0).TDO.driven := io.cpu_jtag(0).TDO.driven


  }.elsewhen(io.selector === jtag0_to_cpu1) {

    /* route jtag1 to cpu 0 and 2 */
    cpu_combinations(1).foreach { j =>
      //j.TCK := io.input_jtag(1).TCK
      j.TDI := io.input_jtag(1).TDI
      j.TMS := io.input_jtag(1).TMS
    }

    io.input_jtag(1).TDO.data := cpu_combinations(1)(0).TDO.data | cpu_combinations(1)(1).TDO.data
    io.input_jtag(1).TDO.driven := cpu_combinations(1)(0).TDO.driven | cpu_combinations(1)(1).TDO.driven

    /* mux clocks */
    tck_sel(0) := true.B
    tck_sel(1) := false.B
    tck_sel(2) := true.B

    /* route jtag0 to cpu1 */
    //io.cpu_jtag(1).TCK := io.input_jtag(0).TCK
    io.cpu_jtag(1).TDI := io.input_jtag(0).TDI
    io.cpu_jtag(1).TMS := io.input_jtag(0).TMS
    io.input_jtag(0).TDO.data := io.cpu_jtag(1).TDO.data
    io.input_jtag(0).TDO.driven := io.cpu_jtag(1).TDO.driven

  }.elsewhen(io.selector === jtag0_to_cpu2) {

    /* route jtag1 to cpu 0 and 1 */
    cpu_combinations(0).foreach { j =>
      //j.TCK := io.input_jtag(1).TCK
      j.TDI := io.input_jtag(1).TDI
      j.TMS := io.input_jtag(1).TMS
    }

    io.input_jtag(1).TDO.data := cpu_combinations(0)(0).TDO.data | cpu_combinations(0)(1).TDO.data
    io.input_jtag(1).TDO.driven := cpu_combinations(0)(0).TDO.driven | cpu_combinations(0)(1).TDO.driven

    /* mux clocks */
    tck_sel(0) := true.B
    tck_sel(1) := true.B
    tck_sel(2) := false.B

    /* route jtag0 to cpu2 */
    //io.cpu_jtag(2).TCK := io.input_jtag(0).TCK
    io.cpu_jtag(2).TDI := io.input_jtag(0).TDI
    io.cpu_jtag(2).TMS := io.input_jtag(0).TMS
    io.input_jtag(0).TDO.data := io.cpu_jtag(2).TDO.data
    io.input_jtag(0).TDO.driven := io.cpu_jtag(2).TDO.driven

  }.otherwise {
    /* zero unused jtag1 outputs */
    io.input_jtag(1).TDO.data := 0.U
    io.input_jtag(1).TDO.driven := 0.U

    tck_sel.foreach { s => s := false.B }

    io.cpu_jtag.foreach { j =>
      //j.TCK := io.input_jtag(0).TCK
      j.TDI := io.input_jtag(0).TDI
      j.TMS := io.input_jtag(0).TMS
    }

    val tdo_data = io.cpu_jtag.aggregate(0.U)({ (res, v) => res | v.TDO.data}, {(p1, p2) => p1 | p2})
    val tdo_driven = io.cpu_jtag.aggregate(0.U)({ (res, v) => res | v.TDO.driven}, {(p1, p2) => p1 | p2})

    io.input_jtag(0).TDO.data := tdo_data
    io.input_jtag(0).TDO.driven := tdo_driven
  }

}


object JTAGRouter extends App {
  if(args.length == 0) {
    chisel3.Driver.execute(args, () => new JTAGRouter(3))
  } else {
    iotesters.Driver.execute(args, () => new JTAGRouter(3)){ c => new JTAGRouterTests(c) }
  }
}
