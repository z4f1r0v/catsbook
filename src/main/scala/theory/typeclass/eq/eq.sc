import cats.Eq

import cats.instances.int._ // for Eq
val eqInt = Eq[Int]

eqInt.eqv(123, 123)
eqInt.eqv(123, 234)

import cats.syntax.eq._ // for === and =!=
123 === 123
123 =!= 234

import cats.instances.int._// for Eq
import cats.instances.option._ // for Eq

Option(1) === Option.empty[Int]
(Some(1) : Option[Int]) === (None : Option[Int])

import cats.syntax.option._ // for some and none

1.some === none[Int]
1.some =!= none[Int]

import java.util.Date
import cats.instances.long._ // for Eq
import cats.instances.string._ // for Eq

implicit val dateEq: Eq[Date] =
  Eq.instance[Date] { (date1, date2) =>
    date1.getTime === date2.getTime
  }
val x = new Date() // now
val y = new Date() // a bit later than now
x === x
x === y

final case class Cat(name: String, age: Int, color: String)

val cat1 = Cat("Garfield",
  38, "orange and black")
val cat2 = Cat("Heathcliff", 33, "orange and black")
val optionCat1 = Option(cat1)
val optionCat2 = Option.empty[Cat]

implicit val catEq: Eq[Cat] =
  Eq.instance[Cat] { (cat1, cat2) =>
    cat1.name === cat2.name && cat1.age === cat2.age &&
      cat1.color === cat2.color
  }
optionCat1 === optionCat2
optionCat1 =!= optionCat2
