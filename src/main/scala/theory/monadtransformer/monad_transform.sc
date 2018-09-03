import cats.data.OptionT
type ListOption[A] = OptionT[List, A]

import cats.instances.list._
// for Monad
import cats.syntax.applicative._ // for pure
val result1: ListOption[Int] = OptionT(List(Option(10)))
// result1: ListOption[Int] = OptionT(List(Some(10)))
val result2: ListOption[Int] = 32.pure[ListOption]

result1.flatMap { (x: Int) =>
  result2.map { (y: Int) =>
    x + y
  }
}

// Alias Either to a type constructor with one parameter:
type ErrorOr[A] = Either[String, A]
// Build our final monad stack using OptionT:
type ErrorOrOption[A] = OptionT[ErrorOr, A]

import cats.instances.either._ // for Monad
val a = 10.pure[ErrorOrOption]
// a: ErrorOrOption[Int] = OptionT(Right(Some(10)))
val b = 32.pure[ErrorOrOption]
// b: ErrorOrOption[Int] = Option
val c = a.flatMap(x => b.map(y => x + y))

import cats.data.{EitherT, OptionT}

import scala.concurrent.Future
type FutureEither[A] = EitherT[Future, String, A]
type FutureEitherOption[A] = OptionT[FutureEither, A]

import cats.instances.future._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

val futureEitherOr: FutureEitherOption[Int] =
  for {
    a <- 10.pure[FutureEitherOption]
    b <- 32.pure[FutureEitherOption]
  } yield a + b

val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
val errorStack2 = 32.pure[ErrorOrOption]

errorStack1.value
errorStack2.value.map(_.getOrElse(-1))

futureEitherOr

val intermediate = futureEitherOr.value

val stack = intermediate.value

Await.result(stack, 1.second)

import cats.data.EitherT

import scala.concurrent.Future
type Response[A] = EitherT[Future, String, A]

import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global

val powerLevels = Map(
  "Jazz"
    -> 6,
  "Bumblebee" -> 8,
  "Hot Rod"
    -> 10
)

def getPowerLevel(ally: String): Response[Int] = {
  powerLevels.get(ally) match {
    case Some(avg) => EitherT.right(Future(avg))
    case None => EitherT.left(Future(s"$ally unreachable"))
  }
}

def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
  for {
    power1 <- getPowerLevel(ally1)
    power2 <- getPowerLevel(ally2)
  } yield (power1 + power2) > 15

import scala.concurrent.Await
import scala.concurrent.duration._

def tacticalReport(ally1: String, ally2: String): String = {
  val stack = canSpecialMove(ally1, ally2).value
  Await.result(stack, 1.second) match {
    case Left(msg) => s"Comms error: $msg"
    case Right(true) =>
      s"$ally1 and $ally2 are ready to roll out!"
    case Right(false) =>
      s"$ally1 and $ally2 need a recharge."
  }
}