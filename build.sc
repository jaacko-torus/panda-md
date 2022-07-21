import mill._
import mill.scalalib._
import mill.scalalib.publish._

object PandaMd extends SbtModule with PublishModule {
  def scalaVersion = "2.13.8"
  def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse:2.2.2",
  )

  def publishVersion = "0.0.1"
  def pomSettings = PomSettings(
    description = "Panda MD",
    organization = "jaackotorus",
    url = "https://github.com/jaacko-torus/panda-md",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("jaacko-torus", "panda-md"),
    developers = Seq(
      Developer(
        "jaacko-torus",
        "Julian A Avar C",
        "https://github.com/jaacko-torus",
      ),
    ),
  )

  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.12",
    )
  }
}
