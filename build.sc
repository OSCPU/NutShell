import mill._, scalalib._
import coursier.maven.MavenRepository

object ivys {
  val scala = "2.12.13"
  val chisel3 = ivy"edu.berkeley.cs::chisel3:3.5.6"
  val chisel3Plugin = ivy"edu.berkeley.cs:::chisel3-plugin:3.5.6"
}

trait CommonModule extends ScalaModule {
  override def scalaVersion = ivys.scala

  override def scalacOptions = Seq("-Xsource:2.11")
}

trait HasChisel3 extends ScalaModule {
  override def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(
      MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
    )
  }
  override def ivyDeps = Agg(ivys.chisel3)
  override def scalacPluginIvyDeps = Agg(ivys.chisel3Plugin)
}

trait HasChiselTests extends SbtModule {
  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def ivyDeps = Agg(ivy"edu.berkeley.cs::chisel-iotesters:1.2+")
  }
}

trait CommonNS extends SbtModule with CommonModule with HasChisel3

object difftest extends CommonNS {
  override def millSourcePath = os.pwd / "difftest"
}

object chiselModule extends CommonNS with HasChiselTests {
  override def millSourcePath = os.pwd

  override def moduleDeps = super.moduleDeps ++ Seq(
    difftest
  )
}
