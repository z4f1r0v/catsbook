import cats.syntax.either._ // for asRight

val either1: Either[String, Int] = Right(10)
val either2: Either[String, Int] = Right(32)
for {
  a <- either1.right
  b <- either2.right
} yield a + b

val a = 3.asRight[String]
// a: Either[String,Int] = Right(3)
val b = 4.asRight[String]
// b: Either[String,Int] = Right(4)
for {
  x <- a
  y <- b
} yield x * x + y * y

def countPositive(nums: List[Int]) =
  nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
    if(num > 0) {
      accumulator.map(_ + 1)
    } else {
      Left("Negative. Stopping!")
    }
  }
countPositive(List(1, 2, 3))

countPositive(List(1, -2, 3))

Either.catchOnly[NumberFormatException]("foo".toInt)
Either.catchNonFatal(sys.error("Badness"))
Either.fromTry(scala.util.Try("foo".toInt))
Either.fromOption[String, Int](None, "Badness")
"Error".asLeft[Int].getOrElse(0)
"Error".asLeft[Int].orElse(2.asRight[String])
(-1).asRight[String].ensure("Must be non-negative!")(_ > 0)
"error".asLeft[Int].recover {
  case str: String => -1
}
// res14: Either[String,Int] = Right(-1)
"error".asLeft[Int].recoverWith {
  case str: String => Right(-1)
}
"foo".asLeft[Int].leftMap(_.reverse)
// res16: Either[String,Int] = Left(oof)
6.asRight[String].bimap(_.reverse, _ * 7)
// res17: Either[String,Int] = Right(42)
"bar".asLeft[Int].bimap(_.reverse, _ * 7)
123.asRight[String]