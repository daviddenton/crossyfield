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
The library provides a single main concept, the `Extractor`, which exposes a couple of `<--?()` methods to provide extraction/validation operations. The result of 
the extraction operation is one of 3 case class instances:

1. `Extracted[T]` - when the object was successfully extracted
2. `NotProvided` - when the object was missing, but was optional
3. `ExtractionFailed(Symbol -> String)` - when the object was invalid or missing when required. Contains the error denoting the failure

The library can be used in a couple of ways, depending on your use case:

#### Option 1: Cross-field validation
The `Extractors` can be used in `for comprehensions` and chained in a graph, with subsequent extractions dependant on those before. The first failure in the extraction chain will short-circuit the operations 
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
  rangeExtraction <--? "2000-01-01,1999-01-01" match {
    case Extracted(value) => println(s"I successfully extracted $value")
    case NotProvided => println(s"Nothing was extracted")
    case ExtractionFailed(e) => println(s"I got this error: $e")
  }
  // results in: I got this error: ('endDate,end date not after start)
```

#### Option 2: Error collection
When you require all of the errors encountered in an extraction to be collected e.g. for Form validation, you can simply provide a list of the extractors to a `Validator` construct. Note 
that in this case, we cannot use a for comprehension (due to it's short-circuiting nature). The result of the operation is either a `Validated` or a `ValidationFailed`:

```scala
val startDate: Extractor[String, LocalDate] = Extractor.mk('startDate, "invalid start date", (s: String) => LocalDate.parse(s))
val endDate: Extractor[String, LocalDate] = Extractor.mk('endDate, "invalid end date", (s: String) => LocalDate.parse(s))

val result = Validator.mk(
  startDate <--? "2000-01-02",
  endDate <--? "2000-01-asd"
) {
  case (start, end) => (start, end)
}
println(result)

// results in: ValidationFailed(List(('endDate,invalid end date)))
```


