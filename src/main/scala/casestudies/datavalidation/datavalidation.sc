import cats.Semigroup
import cats.syntax.either._
import cats.syntax.semigroup._

final case class CheckF[E, A](func: A => Either[E, A]) {

  def apply(value: A): Either[E, A] = func(value)

  def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
    CheckF { value =>
      (this (value), that(value)) match {
        case (Left(v), Left(v2)) => (v |+| v2).asLeft
        case (Left(v), Right(v2)) => v.asLeft
        case (Right(v), Left(v2)) => v2.asLeft
        case (Right(v), Right(v2)) => value.asRight
      }
    }
}

sealed trait Check1[E, A] {

  def and(that: Check1[E, A]): Check1[E, A] =
    And1(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] =
    this match {
      case Pure1(func) =>
        func(a)
      case And1(left, right) =>
        (left(a), right(a)) match {
          case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
          case (Left(e), Right(_)) => e.asLeft
          case (Right(_), Left(e)) => e.asLeft
          case (Right(a1), Right(a2)) => a.asRight
        }
    }
}

final case class Pure1[E, A](
  func: A => Either[E, A]) extends Check1[E, A]

final case class And1[E, A](
  left: Check1[E, A],
  right: Check1[E, A]) extends Check1[E, A]

import cats.instances.list._ // for Semigroup

val a: Check1[List[String], Int] =
  Pure1 { v =>
    if (v > 2) v.asRight
    else List("Must be > 2").asLeft
  }

val b: Check1[List[String], Int] =
  Pure1 { v =>
    if (v < -2) v.asRight
    else List("Must be < -2").asLeft
  }

val check: Check1[List[String], Int] =
  a and b

check(5)
check(0)

import cats.Semigroup
import cats.data.Validated
import cats.syntax.semigroup._ // for |+|
import cats.syntax.apply._ // for mapN
import cats.data.Validated._ // for Valid and Invalid
import cats.syntax.validated._ // for valid and invalid

sealed trait Predicate[E, A] {

  import Predicate._

  def and(that: Predicate[E, A]): Predicate[E, A] =
    And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] =
    Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(func) =>
        func(a)
      case And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)
      case Or(left, right) =>
        left(a) match {
          case Valid(_) => Valid(a)
          case Invalid(e1) =>
            right(a) match {
              case Valid(a2) => Valid(a)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
    }
}

object Predicate {

  final case class And[E, A](
    left: Predicate[E, A],
    right: Predicate[E, A]) extends Predicate[E, A]

  final case class Or[E, A](
    left: Predicate[E, A],
    right: Predicate[E, A]) extends Predicate[E, A]

  final case class Pure[E, A](
    func: A => Validated[E, A]) extends Predicate[E, A]

  def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] =
    Pure(f)

  def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
    Pure(a => if (fn(a)) a.valid else err.invalid)
}

sealed trait Check[E, A, B] {

  import Check._

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](func: B => C): Check[E, A, C] =
    Map(this, func)

  def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] =
    FlatMap(this, func)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
    AndThen(this, that)
}


object Check {

  final case class PurePredicate[E, A](
    pred: Predicate[E, A]) extends Check[E, A, A] {
    def apply(a: A)
      (implicit s: Semigroup[E]): Validated[E, A] =
      pred(a)
  }

  final case class Pure[E, A, B](
    func: A => Validated[E, B]) extends Check[E, A, B] {
    def apply(a: A)
      (implicit s: Semigroup[E]): Validated[E, B] =
      func(a)
  }

  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
    PurePredicate(pred)

  def apply[E, A, B]
  (func: A => Validated[E, B]): Check[E, A, B] =
    Pure(func)

  final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C)
    extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).map(func)
  }

  final case class FlatMap[E, A, B, C](check: Check[E, A, B],
    func: B => Check[E, A, C])
    extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => {
        val value: Check[E, A, C] = func(b)
        value(a).toEither
      }))
  }

  final case class AndThen[E, A, B, C](check: Check[E, A, B],
    check2: Check[E, B, C])
    extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => check2(b).toEither))
  }

}

import cats.data.{NonEmptyList, Validated}

type Errors = NonEmptyList[String]
def error(s: String): NonEmptyList[String] =
  NonEmptyList(s, Nil)

def longerThan(n: Int): Predicate[Errors, String] =
  Predicate.lift(
    error(s"Must be longer than $n characters"),
    str => str.length > n)
val alphanumeric: Predicate[Errors, String] =
  Predicate.lift(
    error(s"Must be all alphanumeric characters"),
    str => str.forall(_.isLetterOrDigit))
def contains(char: Char): Predicate[Errors, String] =
  Predicate.lift(
    error(s"Must contain the character $char"),
    str => str.contains(char))
def containsOnce(char: Char): Predicate[Errors, String] =
  Predicate.lift(
    error(s"Must contain the character $char only once"),
    str => str.count(c => c == char) == 1)

def checkUsername(username: String): Validated[Errors, String] = {
  Check(longerThan(8) and alphanumeric)(username)
}

checkUsername("havaNaUnana")

val splitEmail: Check[Errors, String, (String, String)] =
  Check(_.split('@') match {
    case Array(name, domain) =>
      (name, domain).validNel[String]
    case other =>
      "Must contain a single @ character".invalidNel[(String, String)]
  })

val checkName: Check[Errors, String, String] =
  Check(longerThan(0))

val checkDomain: Check[Errors, String, String] =
  Check(longerThan(3) and contains('.'))

val joinEmail: Check[Errors, (String, String), String] =
  Check { case (left, right) =>
    (checkName(left), checkDomain(right)).mapN(_ + '@' + _)
  }

def checkEmail(email: String) = {
  val rules = splitEmail andThen joinEmail
  rules(email)
}

checkEmail("alal@aa.s")
checkEmail("alal@aa")
checkEmail("@aa.as")
checkEmail("aa.as")
