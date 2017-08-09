

scalaVersion := "2.12.2"



/**
  * http4s is moving away from Scalaz and to fs2/cats which is why I have opted to use Cats and Monix
  * instead of Scalaz.
  * */
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.9.0"
)
