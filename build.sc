import mill._
import scalalib._

object eepyu extends ScalaModule {
  val spinalVersion = "1.10.0"
  def scalaVersion = "2.13.12"

  def ivyDeps = Agg(
    ivy"com.github.spinalhdl::spinalhdl-core:$spinalVersion",
    ivy"com.github.spinalhdl::spinalhdl-lib:$spinalVersion"
  )
  def scalacPluginIvyDeps = Agg(ivy"com.github.spinalhdl::spinalhdl-idsl-plugin:$spinalVersion")

  object test extends ScalaTests {
    override def ivyDeps =
      Agg(ivy"org.scalatest::scalatest:3.2.14", ivy"com.carlosedp::riscvassembler:1.9.1", ivy"net.fornwall:jelf:0.9.0")
    override def testFramework = "org.scalatest.tools.Framework"
  }
}
