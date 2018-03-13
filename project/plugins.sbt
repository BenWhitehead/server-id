resolvers ++= Seq(
  "sbt-plugin-releases-repo" at "http://repo.scala-sbt.org/scalasbt/sbt-plugin-releases"
)

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.2")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
