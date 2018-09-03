import cats.Show

import cats.instances.int._
import cats.instances.string._

val showInt: Show[Int] = Show.apply[Int]
val showString: Show[String] = Show.apply[String]

val intAsString: String = showInt.show(123)
val stringAsString: String = showString.show("abc")

import cats.syntax.show._

val shownInt = 123.show
val shownString = "abc".show

import cats.Show
import cats.instances.int._// for Show
import cats.instances.string._ // for Show
import cats.syntax.show._ // for show

final case class Cat(name: String, age: Int, color: String)

implicit val catShow = Show.show[Cat] { cat =>
  val name = cat.name.show
  val age = cat.age.show
  val color = cat.color.show
  s"$name is a $age year-old $color cat."
}

println(Cat("Garfield", 38, "ginger and black").show)