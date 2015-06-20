---
title: http4s Quickstart
---

http4s is a Scala library for for building HTTP servers and clients
with functional programming principles.

# Creating the build

This tutorial assumes a working [sbt][sbt] installation.  Create an
empty directory for your project and add the follwing to file
`build.sbt`:

```scala
scalaVersion := "2.11.5"

lazy val Http4sVersion = "0.8.2"

libraryDependencies ++= Seq(
  "org.http4s"  %% "http4s-blazeserver"   % Http4sVersion,
  "org.http4s"  %% "http4s-blazeclient"   % Http4sVersion,
  "org.http4s"  %% "http4s-dsl"           % Http4sVersion,
)
```

[sbt]: http://www.scala-sbt.org/0.13/tutorial/index.html

# A first HTTP service

An HTTP service in http4s is represented by the `HttpService` type,
which is a function `Request => Task[Option[Response]]`.
`HttpService`s can conveniently be constructed from a partial function
literal (i.e., case statements) applied to the `HttpService` object:

```tut:silent
import org.http4s._
import org.http4s.dsl._
import org.http4s.server._

val service = HttpService {
  case GET -> Root / "hello" / name =>
    Ok(s"Hello, ${name}.")
}
```

