scalaVersion := "2.10.1"

organization := "info.folone"

name := "abstract-nonsence"

version := "0.1-SNAPSHOT"

// Scalaz
libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core"               % "7.0.0",
  "org.scalaz"     %% "scalaz-effect"             % "7.0.0",
  "org.scalaz"     %% "scalaz-typelevel"          % "7.0.0",
  "org.scalaz"     %% "scalaz-scalacheck-binding" % "7.0.0" % "test"
)

// Shapeless
libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "1.2.4"
)

// Spire
libraryDependencies ++= Seq(
  "org.spire-math" %% "spire"                    % "0.4.0",
  "org.spire-math" %% "spire-scalacheck-binding" % "0.4.0" % "test"
)

// Scalaz-contrib
libraryDependencies ++= Seq(
  "org.typelevel" %% "scalaz-contrib-210"        % "0.1.4",
  "org.typelevel" %% "scalaz-contrib-validation" % "0.1.4",
  "org.typelevel" %% "scalaz-contrib-undo"       % "0.1.4",
  "org.typelevel" %% "scalaz-dispatch"           % "0.1.4",
  "org.typelevel" %% "scalaz-lift"               % "0.1.4",
  "org.typelevel" %% "scalaz-nscala-time"        % "0.1.4",
  "org.typelevel" %% "scalaz-spire"              % "0.1.4"
)

// Shapeless-contrib
libraryDependencies ++= Seq(
  "org.typelevel" %% "shapeless-scalacheck" % "0.1.1",
  "org.typelevel" %% "shapeless-spire"      % "0.1.1",
  "org.typelevel" %% "shapeless-scalaz"     % "0.1.1"
)

scalacOptions += "-feature"
