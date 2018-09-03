import cats.instances.function._
import cats.syntax.functor._

val func1: Int => Double =
  (x: Int) => x.toDouble

val func2: Double => Double =
  (y: Double) => y * 2
(func1 map func2) (1)
// composition using map
// res7: Double = 2.0
(func1 andThen func2) (1) // composition using andThen
// res8: Double = 2.0
func2(func1(1))

val func =
  ((x: Int) => x.toDouble).
    map(x => x + 1).
    map(x => x * 2).
    map(x => x + "!")
func(123)

import cats.Functor
import cats.instances.list._
// for Functor
import cats.instances.option._ // for Functor
val list1 = List(1, 2, 3)
// list1: List[Int] = List(1, 2, 3)
val list2 = Functor[List].map(list1)(_ * 2)
// list2: List[Int] = List(2, 4, 6)
val option1 = Option(123)
// option1: Option[Int] = Some(123)
val option2 = Functor[Option].map(option1)(_.toString)

val func12 = (x: Int) => x + 1
// func: Int => Int = <function1>
val liftedFunc = Functor[Option].lift(func12)
liftedFunc(Option(1))

import cats.instances.function._ // for Functor
import cats.syntax.functor._

// for map
val func11 = (a: Int) => a + 1
val func21 = (a: Int) => a * 2
val func31 = (a: Int) => a + "!"
val func41 = func11.map(func21).map(func31)
func41(123)

import scala.language.higherKinds

def doMath[F[_]](start: F[Int])
                (implicit functor: Functor[F]): F[Int] =
  start.map(n => n + 1 * 2)

import cats.instances.option._ // for Functor
import cats.instances.list._
// for Functor
doMath(Option(20))
// res3: Option[Int] = Some(22)
doMath(List(1, 2, 3))

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

import cats.Functor

implicit val treeFunctor: Functor[Tree] =
  new Functor[Tree] {
    def map[A, B](tree: Tree[A])(func: A => B): Tree[B] =
      tree match {
        case Branch(left, right) =>
          Branch(map(left)(func), map(right)(func))
        case Leaf(value) =>
          Leaf(func(value))
      }
  }

// smart constructor
object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)
}

Tree.leaf(100).map(_ * 2)
// res10: wrapper.Tree[Int] = Leaf(200)
Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2)

trait Printable[A] {
  self =>
  def format(value: A): String
  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String =
        self.format(func(value))
    }
}
def format[A](value: A)(implicit p: Printable[A]): String =
  p.format(value)

final case class Box[A](value: A)

implicit def boxPrintable[A](implicit p: Printable[A]) =
  new Printable[Box[A]] {
    def format(box: Box[A]): String =
      p.format(box.value)
  }
//
//implicit def boxPrintable[A](implicit p: Printable[A]) =
//  p.contramap[Box[A]](_.value)

implicit val stringPrintable: Printable[String] =
  new Printable[String] {
    def format(value: String): String =
      "\"" + value + "\""
  }
implicit val booleanPrintable: Printable[Boolean] =
  new Printable[Boolean] {
    def format(value: Boolean): String =
      if(value) "yes" else "no"
  }
format("hello")
// res3: String = "hello"
format(true)
// res4: String = yes

format(Box("hello world"))
// res5: String = "hello world"
format(Box(true))

trait Codec[A] {
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    val self = this
    new Codec[B] {
      def encode(value: B): String =
        self.encode(enc(value))
      def decode(value: String): B =
        dec(self.decode(value))
    }
  }
}

def encode[A](value: A)(implicit c: Codec[A]): String =
  c.encode(value)
def decode[A](value: String)(implicit c: Codec[A]): A =
  c.decode(value)

implicit val stringCodec: Codec[String] =
  new Codec[String] {
    def encode(value: String): String = value
    def decode(value: String): String = value
  }

implicit val intCodec: Codec[Int] =
  stringCodec.imap(_.toInt, _.toString)
implicit val booleanCodec: Codec[Boolean] =
  stringCodec.imap(_.toBoolean, _.toString)
implicit val doubleCodec: Codec[Double] =
  stringCodec.imap[Double](_.toDouble, _.toString)
implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
  c.imap[Box[A]](Box(_), _.value)

encode(123.4)
// res0: String = 123.4
decode[Double]("123.4")
// res1: Double = 123.4
encode(Box(123.4))
// res2: String = 123.4
decode[Box[Double]]("123.4")
// res3: Box[Double] = Box(123.