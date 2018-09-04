import cats.data.Kleisli
import cats.instances.list._

val step1: Kleisli[List, Int, Int] =
  Kleisli(x => List(x + 1, x - 1))
val step2: Kleisli[List, Int, Int] =
  Kleisli(x => List(x, -x))
val step3: Kleisli[List, Int, Int] =
  Kleisli(x => List(x * 2, x / 2))

val pipeline = step1 andThen step2 andThen step3
val pipeline2 = step1 andThen step2

step1.run(20)
pipeline2.run(20)

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

  def run(implicit s: Semigroup[E]): A => Either[E, A] =
    (a: A) => this(a).toEither

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

type Result[A] = Either[Errors, A]
type Check[A, B] = Kleisli[Result, A, B]

// Create a check from a function:
def check[A, B](func: A => Result[B]): Check[A, B] =
  Kleisli(func)

// Create a check from a Predicate:
def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] =
  Kleisli[Result, A, A](pred.run)

def checkUsername(username: String): Result[String] = {
  checkPred(longerThan(8) and alphanumeric).run(username)
}


import cats.instances.either._
import cats.instances.list._

val splitEmail: Check[String, (String, String)] =
  check(_.split('@') match {
    case Array(name, domain) =>
      Right((name, domain))
    case _ =>
      Left(error("Must contain a single @ character"))
  })

val checkName: Check[String, String] =
  checkPred(longerThan(0))

val checkDomain: Check[String, String] =
  checkPred(longerThan(3) and contains('.'))

val joinEmail: Check[(String, String), String] =
  check { case (left, right) =>
    (checkName(left), checkDomain(right)).mapN(_ + '@' + _)
  }

def checkEmail(email: String) = {
  val rules = splitEmail andThen joinEmail
  rules.run(email)
}

checkEmail("alal@aa.s")
checkEmail("alal@aa")
checkEmail("@aa.as")
checkEmail("aa.as")
checkUsername("havaNaUnana")
