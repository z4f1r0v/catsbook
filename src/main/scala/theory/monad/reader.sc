import cats.data.Reader

case class Cat(name: String, favoriteFood: String)

// defined class Cat
val catName: Reader[Cat, String] =
  Reader(cat => cat.name)

catName.run(Cat("Garfield", "lasagne"))

val greetKitty: Reader[Cat, String] =
  catName.map(name => s"Hello ${name}")

greetKitty.run(Cat("Heathcliff", "junk food"))

val feedKitty: Reader[Cat, String] =
  Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")
val greetAndFeed: Reader[Cat, String] =
  for {
    greet <- greetKitty
    feed <- feedKitty
  } yield s"$greet. $feed."

greetAndFeed(Cat("Garfield", "lasagne"))
greetAndFeed(Cat("Heathcliff", "junk food"))

case class Db(
               usernames: Map[Int, String],
               passwords: Map[String, String]
             )

type DbReader[A] = Reader[Db, A]

def findUsername(userId: Int): DbReader[Option[String]] =
  Reader(db => db.usernames.get(userId))

def checkPassword(username: String, password: String): DbReader[Boolean] =
  Reader(db => db.passwords.get(username).contains(password))

import cats.syntax.applicative._ // for pure

def checkLogin(userId: Int, password: String): DbReader[Boolean] =
  for {
    username
      <- findUsername(userId)
    passwordOk <- username.map { username =>
      checkPassword(username, password)
    }.getOrElse {
      false.pure[DbReader]
    }
  } yield passwordOk

val users = Map(
  1 -> "dade",
  2 -> "kate",
  3 -> "margo"
)
val passwords = Map(
  "dade" -> "zerocool",
  "kate" -> "acidburn",
  "margo" -> "secret"
)

val db = Db(users, passwords)
checkLogin(1, "zerocool").run(db)
checkLogin(4, "davinci").run(db)

//• we are construc ng a batch program that can easily be represented by a func on;
// • we need to defer injec on of a known parameter or set of parameters;
// • we want to be able to test parts of the program in isolation.
