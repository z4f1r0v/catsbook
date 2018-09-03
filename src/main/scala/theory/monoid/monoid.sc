import cats.Monoid
import cats.Semigroup

import cats.instances.string._ // for Monoid

Monoid[String].combine("Hi ", "there")
Monoid[String].empty

import cats.Semigroup
Semigroup[String].combine("Hi ", "there")

import cats.instances.int._
import cats.instances.option._ // for Monoid

val a = Option(22)
val b = Option(20)

Monoid[Option[Int]].combine(a, b)

import cats.instances.string._ // for Monoid
import cats.syntax.semigroup._ // for |+|
val stringResult = "Hi " |+| "there" |+| Monoid[String].empty
import cats.instances.int._ // for Monoid
val intResult = 1 |+| 2 |+| Monoid[Int].empty

//def add(items: List[Int]): Int =
//  items.foldLeft(0)(_ + _)

import cats.Monoid
import cats.instances.int._
// for Monoid
import cats.syntax.semigroup._ // for |+|
def add(items: List[Int]): Int =
  items.foldLeft(Monoid[Int].empty)(_ |+| _)

import cats.Monoid
import cats.instances.int._
// for Monoid
import cats.syntax.semigroup._ // for |+|
def add[A: Monoid](items: List[A]): A =
  items.foldLeft(Monoid[A].empty)(_ |+| _)

add(List(1, 2, 3))
// res9: Int = 6
import cats.instances.option._ // for Monoid
add(List(Some(1), None, Some(2), None, Some(3)))

case class Order(totalCost: Double, quantity: Double)

implicit val monoid: Monoid[Order] = new Monoid[Order] {
  def combine(o1: Order, o2: Order) =
    Order(
      o1.totalCost + o2.totalCost,
      o1.quantity + o2.quantity
    )
  def empty = Order(0, 0)
}