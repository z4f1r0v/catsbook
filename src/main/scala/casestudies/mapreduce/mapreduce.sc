import cats.Monoid
import cats.instances.int._
import cats.syntax.semigroup._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future} // for |+|

/** Single-threaded map-reduce function.
  * Maps `func` over `values` and reduces using a `Monoid[B]`.
  */
def foldMap[A, B : Monoid](as: Vector[A])(func: A => B): B =
  as.map(func).foldLeft(Monoid[B].empty)(_ |+| _)

val result: Future[Int] =
  parallelFoldMap((1 to 1000000).toVector)(identity)
Await.result(result, 1.second)

def parallelFoldMap[A, B: Monoid]
(values: Vector[A])
(func: A => B): Future[B] = {
  val numCores
  = Runtime.getRuntime.availableProcessors
  val groupSize = (1.0 * values.size / numCores).ceil.toInt
  val groups: Iterator[Vector[A]] =
    values.grouped(groupSize)
  val futures: Iterator[Future[B]] =
    groups.map(group => Future(foldMap(group)(func)))
  Future.sequence(futures) map { iterable =>
    iterable.foldLeft(Monoid[B].empty)(_ |+| _)
  }
}
val result1: Future[Int] =
  parallelFoldMap((1 to 1000000).toVector)(identity)
Await.result(result1, 1.second)

import cats.Monoid
import cats.instances.future._
import cats.instances.int._
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.traverse._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
def parallelFoldMap1[A, B: Monoid]
(values: Vector[A])
(func: A => B): Future[B] = {
  val numCores
  = Runtime.getRuntime.availableProcessors
  val groupSize = (1.0 * values.size / numCores).ceil.toInt
  values
    .grouped(groupSize)
    .toVector
    .traverse(group => Future(group.toVector.foldMap(func)))
    .map(_.combineAll)
}
val future: Future[Int] =
  parallelFoldMap1((1 to 1000).toVector)(_ * 1000)
Await.result(future, 1.second)

