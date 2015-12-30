name := "request-response-model"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.apache.httpcomponents" % "httpclient" % "4.5.1",
  "org.scalatest" %% "scalatest" % "2.2.5" % "test",
  "com.github.tomakehurst" % "wiremock" % "1.58" % "test"
)
