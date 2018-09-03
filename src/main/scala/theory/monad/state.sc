import cats.data.State

val a = State[Int, String] { state =>
  (state, s"The state is $state")
}

val (state1, result1) = a.run(10).value
val state = a.runS(10).value
val result = a.runA(10).value

val step1 = State[Int, String] { num =>
  val ans = num + 1
  (ans, s"Result of step1: $ans")
}

val step2 = State[Int, String] { num =>
  val ans = num * 2
  (ans, s"Result of step2: $ans")
}

val both = for {
  a <- step1
  b <- step2
} yield (a, b)

val (state2, result2) = both.run(20).value

val getDemo = State.get[Int]
getDemo.run(10).value

val setDemo = State.set[Int](30)
setDemo.run(10).value

val pureDemo = State.pure[Int, String]("Result")
pureDemo.run(10).value

val inspectDemo = State.inspect[Int, String](_ + "!")
inspectDemo.run(10).value

val modifyDemo = State.modify[Int](_ + 1)
modifyDemo.run(10).value

import State._

val program: State[Int, (Int, Int, Int)] = for {
  a <- get[Int]
  _ <- set[Int](a + 1)
  b <- get[Int]
  _ <- modify[Int](_ + 1)
  c <- inspect[Int, Int](_ * 1000)
} yield (a, b, c)
// program: cats.data

val (state3, result3) = program.run(1).value

import cats.data.State
type CalcState[A] = State[List[Int], A]

def evalOne(sym: String): CalcState[Int] =
  sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)
  }

def operand(num: Int): CalcState[Int] =
  State[List[Int], Int] { stack =>
    (num :: stack, num)
  }

def operator(func: (Int, Int) => Int): CalcState[Int] =
  State[List[Int], Int] {
    case a :: b :: tail =>
      val ans = func(a, b)
      (ans :: tail, ans)
    case _ =>
      sys.error("Fail!")
  }

evalOne("42").runA(Nil).value

val program1 = for {
  _ <- evalOne("1")
  _ <- evalOne("2")
  ans <- evalOne("+")
} yield ans

program1.runA(Nil).value

import cats.syntax.applicative._ // for pure
def evalAll(input: List[String]): CalcState[Int] =
  input.foldLeft(0.pure[CalcState]) { (a, b) =>
    a.flatMap(_ => evalOne(b))
  }

val program2 = evalAll(List("1", "2", "+", "3", "*"))

program2.runA(Nil).value


val program3 = for {
  _ <- evalAll(List("1", "2", "+"))
  _ <- evalAll(List("3", "4", "+"))
  ans <- evalOne("*")
} yield ans

program3.runA(Nil).value

def evalInput(input: String): Int =
  evalAll(input.split(" ").toList).runA(Nil).value
evalInput("1 2 + 3 4 + *")