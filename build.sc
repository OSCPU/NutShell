import mill._, scalalib._
import coursier.maven.MavenRepository

object ivys {
  val scala = "2.13.12"
  val chisel = ivy"org.chipsalliance::chisel:6.0.0-RC1"
  val chiselPlugin = ivy"org.chipsalliance:::chisel-plugin:6.0.0-RC1"
}

trait CommonModule extends ScalaModule {
  override def scalaVersion = ivys.scala

  override def scalacOptions = Seq("-Ymacro-annotations")
}

trait HasChisel extends ScalaModule {
  override def ivyDeps = Agg(ivys.chisel)
  override def scalacPluginIvyDeps = Agg(ivys.chiselPlugin)
}

trait CommonNS extends SbtModule with CommonModule with HasChisel

object difftest extends CommonNS {
  override def millSourcePath = os.pwd / "difftest"
}

object generator extends CommonNS {

  override def millSourcePath = os.pwd

  override def moduleDeps = super.moduleDeps ++ Seq(
    difftest
  )

  object test extends SbtModuleTests with TestModule.ScalaTest

}
