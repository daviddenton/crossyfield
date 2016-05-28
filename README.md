# crossyfield

<a href="https://travis-ci.org/daviddenton/crossyfield"><img src="https://travis-ci.org/daviddenton/crossyfield.svg?branch=master"/></a>&nbsp;&nbsp;&nbsp;
<a href='https://coveralls.io/github/daviddenton/crossyfield?branch=master'><img src='https://coveralls.io/repos/github/daviddenton/crossyfield/badge.svg?branch=master' alt='Coverage Status' /></a>&nbsp;&nbsp;&nbsp;
<a href='https://bintray.com/daviddenton/maven/crossyfield/_latestVersion'><img src='https://api.bintray.com/packages/daviddenton/maven/crossyfield/images/download.svg' alt='Download' /></a>

This library attempts to provide a nice way of performing cross-field validation in Scala `for comprehensions`.

## Get it
The library is hosted in both Maven Central and JCenter. Add the following lines to ```build.sbt```:

```scala
resolvers += "JCenter" at "https://jcenter.bintray.com"
libraryDependencies += "io.github.daviddenton" %% "crossyfield" % "1.2.0"
```

Crossyfield has no dependencies. 

## Learn it
The library provides a single concept, the `Extractor`, which exposes a couple of `<--?()` methods to provide extraction/validation operations. The result of 
the extraction operation is one of 3 case class instances:

1. `Extracted[T]` - when the object was successfully extracted
2. `NotProvided` - when the object was missing, but was optional
3. `Invalid(Seq(Symbol -> String))` - when the object was invalid or missing when required. Contains a sequence of errors denoting the failures

The `Extractors` can be used in `for comprehensions` and chained in a graph. The first failure in the extraction chain will short-circuit the operations 
and return an `Invalid` instance containing the error. Extractors can either just check for presence of well formatted values, or optionally apply validation rules on the result.

Below is an example using a custom date range which is extracted from a CSV string. The implementation checks that the optional range end date is after the start date:

```scala
case class Range(startDate: LocalDate, endDate: Option[LocalDate])

// Extractor[FromType, ToType]
val startDate: Extractor[String, LocalDate] = Extractor.mk('startDate, "invalid start date", (s: String) => LocalDate.parse(s))
val endDate: Extractor[String, LocalDate] = Extractor.mk('endDate, "invalid end date", (s: String) => LocalDate.parse(s))

val rangeExtraction: Extractor[String, Range] = Extractor.mk('range) {
  input: String => {
    val parts = input.split(",")

    for {
      startDate <- startDate <--? parts(0)
      endDate <- endDate <--?(parts(1), "end date not after start", e => startDate.map(s => e.isAfter(s)).getOrElse(true))
    } yield Range(startDate.get, endDate)
  }
}

// now we can actually use our extractor on some input
rangeExtraction <--? "2000-01-01,2002-01-01" match {
    case Extracted(value) => println(s"I successfully extracted $value")
    case NotProvided => println(s"Nothing was extracted")
    case Invalid(e) => println(s"I got these errors: $e")
}
```