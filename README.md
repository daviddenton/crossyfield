# crossyfield

<a href="https://travis-ci.org/daviddenton/crossyfield"><img src="https://travis-ci.org/daviddenton/crossyfield.svg?branch=master"/></a>&nbsp;&nbsp;&nbsp;
<a href='https://coveralls.io/github/daviddenton/crossyfield?branch=master'><img src='https://coveralls.io/repos/github/daviddenton/crossyfield/badge.svg?branch=master' alt='Coverage Status' /></a>&nbsp;&nbsp;&nbsp;
<a href='https://bintray.com/daviddenton/maven/crossyfield/_latestVersion'><img src='https://api.bintray.com/packages/daviddenton/maven/crossyfield/images/download.svg' alt='Download' /></a>

This library provides a way of performing generic cross-field validation in Scala `for comprehensions`.

## Get it
Crossyfield has no dependencies. 

Add the following lines to ```build.sbt```:

```scala
resolvers += "JCenter" at "https://jcenter.bintray.com"
libraryDependencies += "io.github.daviddenton" %% "crossyfield" % "1.0.0"
```

## Learn it
The library provides a single concept, the `Validator`, which exposes a couple of `<--?()` methods to provide validation operations.

```scala

case class Range(startDate: LocalDate, middleDate: Option[LocalDate], endDate: LocalDate)

```