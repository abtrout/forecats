import AssemblyKeys._

organization  := "io.forecats"

version       := "0.1"

scalaVersion  := "2.11.2"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  val sprayV = "1.3.2"
  Seq(
    "com.maxmind.geoip2"  %   "geoip2"            % "2.2.0",
    "com.typesafe.akka"   %%  "akka-actor"        % "2.3.6",
    "io.spray"            %%  "spray-can"         % sprayV,
    "io.spray"            %%  "spray-caching"     % sprayV,
    "io.spray"            %%  "spray-client"      % sprayV,
    "io.spray"            %%  "spray-routing"     % sprayV,
    "io.argonaut"         %%  "argonaut"          % "6.1",
    "org.scalikejdbc"     %%  "scalikejdbc"       % "2.2.8",
    "org.xerial"          %   "sqlite-jdbc"       % "3.7.2"
  )
}

seq(Revolver.settings: _*)

seq(jsSettings: _*)

(resourceGenerators in Compile) <+= (JsKeys.js in Compile)

(compile in Compile) <<= compile in Compile dependsOn (JsKeys.js in Compile)

seq(assemblySettings: _*)

jarName in assembly := "forecats.jar"

mainClass in assembly := Some("io.forecats.Boot")

mergeStrategy in assembly <<= (mergeStrategy in assembly) { old => {
  case PathList("www", xs @ _*) => MergeStrategy.discard
  case x => old(x)
}}
