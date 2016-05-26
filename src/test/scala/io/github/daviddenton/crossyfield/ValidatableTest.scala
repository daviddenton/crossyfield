package io.github.daviddenton.crossyfield

import org.scalatest._

class ValidatableTest extends FunSpec with ShouldMatchers {

  case class Example(a: Option[String], b: Option[String], c: Int)

  case class WrappedExample(d: Option[Example], e: Int)

  //  describe("Validatable") {
  //    it("does not short circuit if all parameters in a for comprehension are optional") {
  //      val ex = Validatable.mk {
  //        request: Request => for {
  //          req <- Query.optional.int("req") <--? request
  //          opt <- Query.optional.int("optional") <--? request
  //        } yield (req, opt)
  //      }
  //
  //      ex <--? Request("/") shouldBe Validated((None, None))
  //    }
  //
  //    it("does not short circuit if last parameter in a for comprehension is optional") {
  //      val ex = Validatable.mk {
  //        request: Request => for {
  //          req <- Query.required.int("req") <--? request
  //          opt <- Query.optional.int("optional") <--? request
  //        } yield (req, opt)
  //      }
  //
  //      ex <--? Request("/?req=123") shouldBe Validated((Some(123), None))
  //    }
  //
  //    describe("non-embedded extraction") {
  //      val int = Query.required.int("name3")
  //      val c = Validatable.mk {
  //        request: Request => for {
  //          name3 <- int.extract(request)
  //          name1 <- Query.optional.string("name1").extract(request)
  //          name2 <- Query.optional.string("name2").extract(request)
  //        } yield Example(name1, name2, name3.get)
  //      }
  //
  //      it("successfully extracts when all parameters present") {
  //        c <--? Request("/?name1=query1&name2=rwer&name3=12") shouldBe Validated(Example(Some("query1"), Some("rwer"), 12))
  //      }
  //
  //      it("successfully extracts when only optional parameters missing") {
  //        c <--? Request("/?name3=123") shouldBe Validated(Example(None, None, 123))
  //      }
  //
  //      it("reports error when not all parameters present") {
  //        c <--? Request("/?name1=query1") shouldBe ValidationFailed(Missing(int))
  //      }
  //    }
  //
  //    it("validation error between parameters") {
  //
  //      case class Range(startDate: LocalDate, middleDate: Option[LocalDate], endDate: LocalDate)
  //
  //      val start = Query.optional.localDate("start")
  //      val middle = Query.optional.localDate("middle")
  //      val end = Query.required.localDate("end")
  //
  //      val c = Validatable.mk {
  //        request: Request => {
  //          for {
  //            startDate <- start <--? request
  //            middleDate <- middle <--?(request, "not after start", (i: LocalDate) => i.isAfter(startDate.get))
  //            endDate <- end <--?(request, "not after start", e => startDate.map(s => e.isAfter(s)).getOrElse(true))
  //          } yield Some(Range(startDate.get, middleDate, endDate.get))
  //        }
  //      }
  //
  //      c <--? Request("/?start=2002-01-01&end=2001-01-01") shouldBe ValidationFailed(InvalidParameter(end, "not after start"))
  //    }
  //
  //    describe("can embed extractables") {
  //      val innerInt = Query.required.int("innerInt")
  //      val outerInt = Query.required.int("outerInt")
  //      val inner = Validatable.mk {
  //        request: Request => for {
  //          name3 <- innerInt.extract(request)
  //          name1 <- Query.optional.string("name1").extract(request)
  //          name2 <- Query.optional.string("name2").extract(request)
  //        } yield Example(name1, name2, name3.get)
  //      }
  //
  //      val outer = Validatable.mk {
  //        request: Request => for {
  //          name4 <- outerInt <--? request
  //          inner <- inner <--? request
  //        } yield WrappedExample(inner, name4.get)
  //      }
  //
  //      it("success") {
  //        outer <--? Request("/?innerInt=123&outerInt=1") shouldBe Validated(WrappedExample(Some(Example(None, None, 123)), 1))
  //      }
  //
  //      it("inner extract fails reports only inner error") {
  //        outer <--? Request("/?outerInt=123") shouldBe ValidationFailed(Missing(innerInt))
  //      }
  //      it("outer extract fails reports only outer error") {
  //        outer <--? Request("/?innerInt=123") shouldBe ValidationFailed(Missing(outerInt))
  //      }
  //    }
  //
  //    describe("falling back to default value") {
  //      it("Validated") {
  //        Validated(true).orDefault(false) shouldBe Validated(true)
  //      }
  //      it("NotProvided") {
  //        NotProvided.orDefault(true) shouldBe Validated(true)
  //      }
  //      it("ValidationFailed") {
  //        val param = Query.required.string("param")
  //        ValidationFailed(Invalid(param)).orDefault(true) shouldBe ValidationFailed(Invalid(param))
  //
  //      }
  //    }
  //
  //    describe("misc methods") {
  //      val invalid = Invalid(Query.optional.string("bob"))
  //      val missing = Missing(Query.optional.string("bob"))
  //      it("flatten") {
  //        Validation.flatten(NotProvided) shouldBe NotProvided
  //        Validation.flatten(Validated(None)) shouldBe NotProvided
  //        Validation.flatten(Validated(Some(1))) shouldBe Validated(1)
  //        Validation.flatten(ValidationFailed(Seq(invalid))) shouldBe ValidationFailed(Seq(invalid))
  //      }
  //      it("combine") {
  //        Validation.combine(Seq(NotProvided, NotProvided)) shouldBe NotProvided
  //        Validation.combine(Seq(NotProvided, Validated(1))) shouldBe NotProvided
  //        Validation.combine(Seq(NotProvided, Validated(1), ValidationFailed(missing), ValidationFailed(invalid))) shouldBe ValidationFailed(Seq(missing, invalid))
  //      }
  //    }

}

}
