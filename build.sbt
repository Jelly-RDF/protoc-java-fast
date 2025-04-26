ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.5"

lazy val protobufJavaV = "4.30.2"
lazy val javapoetV = "0.7.0"

lazy val runtime = (project in file("runtime"))
  .settings(
    name := "protoc-java-fast-runtime",
    libraryDependencies ++= Seq(
      "com.google.protobuf" % "protobuf-java" % protobufJavaV,
    )
  )

lazy val generator = (project in file("generator"))
  .settings(
    name := "protoc-java-fast",
    libraryDependencies ++= Seq(
      "com.google.protobuf" % "protobuf-java" % protobufJavaV,
      "com.palantir.javapoet" % "javapoet" % javapoetV,
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    )
  ).dependsOn(runtime)

lazy val testProject = (project in file("test-project"))
  .settings(
    name := "protoc-java-fast-test",
  ).dependsOn(generator)
