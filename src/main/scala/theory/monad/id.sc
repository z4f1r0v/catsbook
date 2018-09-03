import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Id, Monad}

import scala.language.higherKinds // for flatMap
def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
  a.flatMap(x => b.map(y => x*x + y*y))


sumSquare(3 : Id[Int], 4 : Id[Int])

"Dave" : Id[String]
// res3: cats.Id[String] = Dave
123 : Id[Int]
// res4: cats.Id[Int] = 123
List(1, 2, 3) : Id[List[Int]]

val a = Monad[Id].pure(3)
// a: cats.Id[Int] = 3
val b = Monad[Id].flatMap(a)(_ + 1)
// b: cats.Id[Int] = 4
import cats.syntax.flatMap._
import cats.syntax.functor._ // for flatMap
for {
  x <- a
  y <- b
} yield x + y
