import cats.Semigroupal
import cats.instances.option._ // for Semigroupal

Semigroupal[Option].product(Some(123), Some("abc"))

Semigroupal[Option].product(None, Some("abc"))

Semigroupal[Option].product(Some(123), None)

Semigroupal.tuple3(Option(1), Option(2), Option(3))
// res3: Option[(Int, Int, Int)] = Some((1,2,3))
Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])

Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
// res5: Option[Int] = Some(6)
Semigroupal.map2(Option(1), Option.empty[Int])(_ + _)

import cats.syntax.apply._

(Option(123), Option("abc")).tupled
(Option(123), Option("abc"), Option(true)).tupled

case class Dog(name: String, born: Int, color: String)

(
  Option("Garfield"),
  Option(1978),
  Option("Orange & black")
).mapN(Dog.apply)

import cats.Monoid
import cats.instances.int._
import cats.instances.invariant._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.apply._ // for imapN

case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])

val tupleToCat: (String, Int, List[String]) => Cat =
  Cat.apply _
val catToTuple: Cat => (String, Int, List[String]) =
  cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

implicit val catMonoid: Monoid[Cat] = (
  Monoid[String],
  Monoid[Int],
  Monoid[List[String]]
).imapN(tupleToCat)(catToTuple)

import cats.syntax.semigroup._ // for |+|
val garfield = Cat("Garfield", 1978, List("Lasagne"))
val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))
garfield |+| heathcliff

import cats.Semigroupal
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.higherKinds
val futurePair = Semigroupal[Future].
  product(Future("Hello"), Future(123))
Await.result(futurePair, 1.second)

val futureCat = (
  Future("Garfield"),
  Future(1978),
  Future(List("Lasagne"))
).mapN(Cat.apply)
Await.result(futureCat, 1.second)

import cats.Semigroupal
import cats.instances.list._ // for Semigroupal
Semigroupal[List].product(List(1, 2), List(3, 4))

import cats.instances.either._ // for Semigroupal
type ErrorOr[A] = Either[Vector[String], A]
Semigroupal[ErrorOr].product(
  Left(Vector("Error 1")),
  Left(Vector("Error 2"))
)
