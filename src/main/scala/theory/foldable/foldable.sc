import cats.Foldable
import cats.instances.list._ // for Foldable
val ints = List(1, 2, 3)
Foldable[List].foldLeft(ints, 0)(_ + _)

import cats.instances.option._ // for Foldable
val maybeInt = Option(123)
Foldable[Option].foldLeft(maybeInt, 10)(_ * _)

import cats.Eval
import cats.Foldable
import cats.instances.stream._ // for Foldable

def bigData = (1 to 100000).toStream

val eval: Eval[Long] =
  Foldable[Stream].
    foldRight(bigData, Eval.now(0L)) { (num, eval) =>
      eval.map(_ + num)
    }
eval.value

import cats.instances.int._ // for Monoid
import cats.instances.string._ // for Monoid
Foldable[List].foldMap(List(1, 2, 3))(_.toString)

import cats.instances.vector._ // for Monoid
val intss = List(Vector(1, 2, 3), Vector(4, 5, 6))
(Foldable[List] compose Foldable[Vector]).combineAll(intss)

import cats.syntax.foldable._ // for combineAll and foldMap
List(1, 2, 3).combineAll
// res16: Int = 6
List(1, 2, 3).foldMap(_.toString)

