import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

val hostnames = List(
  "alpha.example.com",
  "beta.example.com",
  "gamma.demo.com"
)

def getUptime(hostname: String): Future[Int] =
  Future(hostname.length * 60)

val allUptimes: Future[List[Int]] =
  hostnames.foldLeft(Future(List.empty[Int])) {
    (accum, host) =>
      val uptime = getUptime(host)
      for {
        accum
          <- accum
        uptime <- uptime
      } yield accum :+ uptime
  }
Await.result(allUptimes, 1.second)

val allUptimes1: Future[List[Int]] =
  Future.traverse(hostnames)(getUptime)
Await.result(allUptimes, 1.second)

import cats.Traverse
import cats.instances.future._ // for Applicative
import cats.instances.list._ // for Traverse

val totalUptime: Future[List[Int]] =
  Traverse[List].traverse(hostnames)(getUptime)
Await.result(totalUptime, 1.second)

val numbers = List(Future(1), Future(2), Future(3))
val numbers2: Future[List[Int]] =
  Traverse[List].sequence(numbers)

Await.result(numbers2, 1.second)

Await.result(numbers2, 1.second)

import cats.syntax.traverse._ // for sequence and traverse
Await.result(hostnames.traverse(getUptime), 1.second)

Await.result(numbers.sequence, 1.second)