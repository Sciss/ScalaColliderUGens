addSbtPlugin("com.eed3si9n" % "sbt-buildinfo"     % "0.10.0")  // provides version information to copy into main class
addSbtPlugin("com.typesafe" % "sbt-mima-plugin"   % "0.8.1")   // binary compatibility testing
addSbtPlugin("ch.epfl.lamp" % "sbt-dotty"         % "0.4.5")   // cross-compile for dotty
addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.6.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs"       % "1.3.0")   // cross-compile for scala.js

