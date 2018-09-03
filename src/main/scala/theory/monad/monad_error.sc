import cats.MonadError
import cats.instances.either._ // for MonadError
type ErrorOr[A] = Either[String, A]
val monadError = MonadError[ErrorOr, String]
val success = monadError.pure(42)
val failure = monadError.raiseError("Badness")
monadError.handleError(failure) {
  case "Badness" =>
    monadError.pure("It's ok")
  case other =>
    monadError.raiseError("It's not ok")
}
monadError.ensure(success)("Number too low!")(_ > 1000)

import cats.syntax.applicative._
// for pure
import cats.syntax.applicativeError._ // for raiseError etc
import cats.syntax.monadError._// for ensure

val successs = 42.pure[ErrorOr]
// success: ErrorOr[Int] = Right(42)
val failuree = "Badness".raiseError[ErrorOr, Int]
success.ensure("Number to low!")(_ > 1000)

import scala.util.Try
import cats.instances.try_._ // for MonadError
val exn: Throwable =   new RuntimeException("It's all gone wrong")
exn.raiseError[Try, Int]
