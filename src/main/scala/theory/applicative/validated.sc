import cats.Semigroupal
import cats.data.Validated
import cats.instances.list._ // for Monoid
type AllErrorsOrA[A] = Validated[List[String], A]
Semigroupal[AllErrorsOrA].product(
  Validated.invalid(List("Error 1")),
  Validated.invalid(List("Error 2"))
)

val v = Validated.Valid(123)
// v: cats.data.Validated.Valid[Int] = Valid(123)
val i = Validated.Invalid(List("Badness"))

val v2 = Validated.valid[List[String], Int](123)
// v: cats.data.Validated[List[String],Int] = Valid(123)
val i2 = Validated.invalid[List[String], Int](List("Badness"))

import cats.syntax.validated._ // for valid and invalid
123.valid[List[String]]
// res2: cats.data.Validated[List[String],Int] = Valid(123)
List("Badness").invalid[Int]

import cats.syntax.applicative._
// for pure
import cats.syntax.applicativeError._ // for raiseError
type ErrorsOr[A] = Validated[List[String], A]
123.pure[ErrorsOr]
// res5: ErrorsOr[Int] = Valid(123)
List("Badness").raiseError[ErrorsOr, Int]

Validated.catchOnly[NumberFormatException]("foo".toInt)
Validated.catchNonFatal(sys.error("Badness"))
Validated.fromTry(scala.util.Try("foo".toInt))
Validated.fromEither[String, Int](Left("Badness"))
Validated.fromOption[String, Int](None, "Badness")

type AllErrorsOr[A] = Validated[String, A]

import cats.instances.string._ // for Semigroup
Semigroupal[AllErrorsOr]

import cats.syntax.apply._ // for tupled
(
  "Error 1".invalid[Int],
  "Error 2".invalid[Int]
).tupled

import cats.instances.vector._ // for Semigroupal
(
  Vector(404).invalid[Int],
  Vector(500).invalid[Int]
).tupled

123.valid.map(_ * 100)

"?".invalid.leftMap(_.toString)

123.valid[String].bimap(_ + "!", _ * 100)

"?".invalid[Int].bimap(_ + "!", _ * 100)

import cats.syntax.either._ // for toValidated

"Badness".invalid[Int]

"Badness".invalid[Int].toEither

"Badness".invalid[Int].toEither.toValidated

41.valid[String].withEither(_.flatMap(n => Right(n + 1)))

"fail".invalid[Int].getOrElse(0)

"fail".invalid[Int].fold(_ + "!!!", _.toString)

case class User(name: String, age: Int)

import cats.data.Validated
type FormData = Map[String, String]
type FailFast[A] = Either[List[String], A]
type FailSlow[A] = Validated[List[String], A]

def getValue(name: String)(data: FormData): FailFast[String] =
  data.get(name).
    toRight(List(s"$name field not specified"))

import cats.syntax.either._ // for catchOnly
type NumFmtExn = NumberFormatException
def parseInt(name: String)(data: String): FailFast[Int] =
  Either.catchOnly[NumFmtExn](data.toInt).
    leftMap(_ => List(s"$name must be an integer"))

def nonBlank(name: String)(data: String): FailFast[String] =
  Right(data).ensure(List(s"$name cannot be blank"))(_.nonEmpty)

def nonNegative(name: String)(data: Int): FailFast[Int] =
  Right(data).ensure(List(s"$name must be non-negative"))(_ >= 0)

def readName(data: FormData): FailFast[String] =
  getValue("name")(data).
    flatMap(nonBlank("name"))

def readAge(data: FormData): FailFast[Int] =
  getValue("age")(data).
    flatMap(nonBlank("age")).
    flatMap(parseInt("age")).
    flatMap(nonNegative("age"))

import cats.instances.list._ // for Semigroupal
import cats.syntax.apply._
// for mapN
def readUser(data: FormData): FailSlow[User] =
  (
    readName(data).toValidated,
    readAge(data).toValidated
  ).mapN(User.apply)
readUser(Map("name" -> "Dave", "age" -> "37"))

readUser(Map("age" -> "-1"))

