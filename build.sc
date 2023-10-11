import mill._, scalalib._
import coursier.maven.MavenRepository

object ivys {
  val scala = "2.13.10"
  val chiselCrossVersions = Map(
    "3.6.0" -> (ivy"edu.berkeley.cs::chisel3:3.6.0", ivy"edu.berkeley.cs:::chisel3-plugin:3.6.0"),
    "6.0.0-M3" -> (ivy"org.chipsalliance::chisel:6.0.0-M3", ivy"org.chipsalliance:::chisel-plugin:6.0.0-M3"),
  )
}

trait CommonModule extends ScalaModule {
  override def scalaVersion = ivys.scala

  override def scalacOptions = Seq("-Ymacro-annotations")
}

trait HasChiselCross extends ScalaModule with Cross.Module[String]{
  override def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(
      MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
    )
  }
  override def ivyDeps = Agg(ivys.chiselCrossVersions(crossValue)._1)
  override def scalacPluginIvyDeps = Agg(ivys.chiselCrossVersions(crossValue)._2)
}

trait HasChiselTests extends SbtModule {
  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def ivyDeps = Agg(ivy"edu.berkeley.cs::chiseltest:0.5.4")
  }
}

trait CommonNS extends SbtModule with CommonModule with HasChiselCross

object difftest extends Cross[CommonNS](ivys.chiselCrossVersions.keys.toSeq){
  override def millSourcePath = os.pwd / "difftest"
}

object chiselModule extends Cross[ChiselModule](ivys.chiselCrossVersions.keys.toSeq)

trait ChiselModule extends CommonNS with HasChiselTests with Cross.Module[String] {
  override def millSourcePath = os.pwd

  override def moduleDeps = super.moduleDeps ++ Seq(
    difftest(crossValue)
  )
}