package jtag_router
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import java.nio.file.{Files, Paths}
import java.io.BufferedOutputStream
import java.io.FileOutputStream


class JTAGRouterTests(c: JTAGRouter) extends PeekPokeTester(c) {

  def asUnsigned(unsignedLong: Long) =
  ((BigInt(unsignedLong >>> 1) << 1) + (unsignedLong & 1) & 0x00000000ffffffffL)

  val rand = scala.util.Random
}

