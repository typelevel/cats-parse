---
id: installation
title: Installation
---

Cats Parse is currently available for Scala 2.13, 2.12, [Scala.js](https://www.scala-js.org) and [Dotty](https://dotty.epfl.ch).


## SBT

```scala
libraryDependencies += "com.typelevel" %% "cats-parse" % "@VERSION@"
```

## Mill

```scala
import mill._, scalalib._

object foo extends ScalaModule {
  def scalaVersion = "2.13.1"

  def ivyDeps = Agg(
    ivy"org.typelevel::cats-parse:@VERSION@",
  )
}
```