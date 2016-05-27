# crossyfield

<a href="https://travis-ci.org/daviddenton/crossyfield"><img src="https://travis-ci.org/daviddenton/crossyfield.svg?branch=master"/></a>&nbsp;&nbsp;&nbsp;
<a href='https://coveralls.io/github/daviddenton/crossyfield?branch=master'><img src='https://coveralls.io/repos/github/daviddenton/crossyfield/badge.svg?branch=master' alt='Coverage Status' /></a>&nbsp;&nbsp;&nbsp;
<a href='https://bintray.com/daviddenton/maven/crossyfield/_latestVersion'><img src='https://api.bintray.com/packages/daviddenton/maven/crossyfield/images/download.svg' alt='Download' /></a>

This library attempts to provide a nice way of performing generic cross-field validation in Scala `for comprehensions`.

## Get it
Add the following lines to ```build.sbt```:

```scala
resolvers += "JCenter" at "https://jcenter.bintray.com"
libraryDependencies += "io.github.daviddenton" %% "crossyfield" % "1.2.0"
```

Crossyfield has no dependencies. 

## Learn it
The library provides a single concept, the `Extractor`, which exposes a couple of `<--?()` methods to provide extraction/validation operations. The result of 
the extraction operation is one of 3 Case classes:
1. `Extracted[T]` - when the object was successfully extracted
2. `NotProvided` - when the object was missing, but was optional
3. `Invalid(Seq(Symbol -> String))` - when the object was invalid or missing when required. Contains a sequence of errors denoting the failures

The `Extractors` can be used in `for comprehensions` and chained in a graph. The first failure in the extraction chain will short-circuit the operations 
and return an `Invalid` instance containing the error.
