final case class GCounter1(counters: Map[String, Int]) {

  def increment(machine: String, amount: Int): GCounter1 = {
    val i = counters.getOrElse(machine, 0)
    GCounter1(counters.updated(machine, i + amount))
  }

  def merge(that: GCounter1): GCounter1 = {
    val updated = counters.map {
      case (k, v) =>
        val max = Math.max(that.counters.getOrElse(k, 0), v)
        (k, max)
    }
    GCounter1(that.counters ++ updated)
  }

  def total: Int = counters.values.sum
}

val intToInt = Map("1" -> 12, "2" -> 3, "3" -> 43)
val intToInt2 = Map("1" -> 3, "2" -> 4, "3" -> 5)
GCounter1(intToInt).merge(GCounter1(intToInt2))

import cats.Monoid

import scala.language.higherKinds

trait BoundedSemiLattice[A] extends Monoid[A] {

  def combine(a1: A, a2: A): A

  def empty: A
}

object BoundedSemiLattice {

  implicit val boundedSemiLatticeInt = new BoundedSemiLattice[Int] {

    override def combine(a1: Int, a2: Int): Int = a1 max a2

    override def empty: Int = 0
  }

  implicit def boundedSemiLatticeSet[A]: BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {

    override def combine(a1: Set[A], a2: Set[A]): Set[A] = a1.union(a2)

    override def empty = Set.empty[A]
  }
}

import cats.instances.list._
import cats.instances.map._
import cats.syntax.semigroup._ // for |+|
import cats.syntax.foldable._

trait GCounter[F[_, _], K, V] {

  def increment(f: F[K, V])(k: K, v: V)
    (implicit m: Monoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])
    (implicit b: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])
    (implicit m: Monoid[V]): V
}

object GCounter {

  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]): GCounter[F, K, V] = counter

  implicit def mapGCounter[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {

    override def increment(f: Map[K, V])(k: K, v: V)(implicit m: Monoid[V]): Map[K, V] = {
      val i = f.getOrElse(k, m.empty)
      f.updated(k, i |+| v)
    }

    override def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] = {
      f1 |+| f2
    }

    override def total(f: Map[K, V])(implicit m: Monoid[V]): V = f.values.toList.combineAll
  }
}


import cats.instances.int._ // for Monoid
val g1 = Map("a" -> 7, "b" -> 3)
val g2 = Map("a" -> 2, "b" -> 5)

val counter = GCounter[Map, String, Int]
val merged = counter.merge(g1, g2)

val total = counter.total(merged)

trait KeyValueStore[F[_, _]] {

  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

  def get[K, V](f: F[K, V])(k: K): Option[V]

  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)

  def values[K, V](f: F[K, V]): List[V]
}

object KeyValueStore {

  implicit val mapKVStore: KeyValueStore[Map] = new KeyValueStore[Map] {
    override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)

    override def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

    override def values[K, V](f: Map[K, V]): List[V] = f.values.toList
  }
}
