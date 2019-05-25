package leongu.myjvm.basic

import java.text.DateFormat._
import java.util.{Date, Locale}

object App {
  def main(args: Array[String]): Unit = {
    val line = "tom 12"
    println(line.split(" "))
  }
}

object FrenchDate {
  def main(args: Array[String]) {
    val now = new Date
    val df = getDateInstance(LONG, Locale.FRANCE)
    println(df format now)
  }
}

object Timer {
  def oncePerSecond(callback: () => Unit) {
    while (true) {
      callback();
      Thread sleep 1000
    }
  }

  def timeFlies() {
    println("time flies like an arrow...")
  }

  def main(args: Array[String]) {
    oncePerSecond(timeFlies)
  }
}

object TimerAnonymous {
  def oncePerSecond(callback: () => Unit) {
    while (true) {
      callback();
      Thread sleep 1000
    }
  }

  def main(args: Array[String]) {
    oncePerSecond(() =>
      println("time flies like an arrow..."))
  }
}


class Complex(real: Double, imaginary: Double) {
  def re() = real

  def im() = imaginary
}

class Complex1(real: Double, imaginary: Double) {
  def re = real

  def im = imaginary

  override def toString() =
    "" + re + (if (im < 0) "" else "+") + im + "i"
}

object TreeTest {

  abstract class Tree

  case class Sum(l: Tree, r: Tree) extends Tree

  case class Var(n: String) extends Tree

  case class Const(v: Int) extends Tree

  type Environment = String => Int

  def eval(t: Tree, env: Environment): Int = t match {
    case Sum(l, r) => eval(l, env) + eval(r, env)
    case Var(n) => env(n)
    case Const(v) => v
  }

  def derive(t: Tree, v: String): Tree = t match {
    case Sum(l, r) => Sum(derive(l, v), derive(r, v))
    case Var(n) if (v == n) => Const(1)
    case _ => Const(0)
  }

  def main(args: Array[String]) {
    val exp: Tree = Sum(Sum(Var("x"), Var("x")), Sum(Const(7), Var("y")))
    val env: Environment = {
      case "x" => 5
      case "y" => 7
    }
    println("Expression: " + exp)
    println("Evaluation with x=5, y=7: " + eval(exp, env))
    println("Derivative relative to x:\n " + derive(exp, "x"))
    println("Derivative relative to y:\n " + derive(exp, "y"))
  }
}

trait Ord {
  def <(that: Any): Boolean
  def <=(that: Any): Boolean = (this < that) || (this == that)
  def >(that: Any): Boolean = !(this <= that)
  def >=(that: Any): Boolean = !(this < that)
}

class MyDate(y: Int, m: Int, d: Int) extends Ord {
  def year = y
  def month = m
  def day = d
  override def toString(): String = year + "-" + month + "-" + day
//  override def equals(that: Any): Boolean =
//    that.isInstanceOf[MyDate] && {
//      val o = that.asInstanceOf[MyDate]
//      o.day == day && o.month == month && o.year == year
//    }
//  def <(that: Any): Boolean = {
//    if (!that.isInstanceOf[MyDate])
//      sys.error("cannot compare " + that + " and a Date")
//
//    val o = that.asInstanceOf[MyDate]
//    (year < o.year) ||
//      (year == o.year && (month < o.month ||
//        (month == o.month && day < o.day)))
//  }
  override def <(that: Any): Boolean = ???
}

class Reference[T] {
  private var contents: T = _
  def set(value: T) { contents = value }
  def get: T = contents
}


//def addThenMultiply(x: Int, y: Int)(multiplier: Int): Int = (x + y) * multiplier
//println(addThenMultiply(1, 2)(3)) // 9

class Point {
  private var _x = 0
  private var _y = 0
  private val bound = 100

  def x = _x
  def x_= (newValue: Int): Unit = {
    if (newValue < bound) _x = newValue else printWarning
  }

  def y = _y
  def y_= (newValue: Int): Unit = {
    if (newValue < bound) _y = newValue else printWarning
  }

  private def printWarning = println("WARNING: Out of bounds")
}
//
//val point1 = new Point
//point1.x = 99
//point1.y = 101 // prints the warning

abstract class A {
  val message: String
}
class B extends A {
  val message = "I'm an instance of class B"
}
trait C extends A {
  def loudMessage = message.toString.toUpperCase()
}
class D extends B with C
//
//val d = new D
//println(d.message)  // I'm an instance of class B
//println(d.loudMessage)  // I'M AN INSTANCE OF CLASS B


abstract class AbsIterator {
  type T
  def hasNext: Boolean
  def next(): T
}
//
//val salaries = Seq(20000, 70000, 40000)
//val doubleSalary = (x: Int) => x * 2
//val newSalaries = salaries.map(doubleSalary) // List(40000, 140000, 80000)
//val newSalaries2 = salaries.map(_ * 2)

object SalaryRaiser {
  private def promotion(salaries: List[Double], promotionFunction: Double => Double): List[Double] =
    salaries.map(promotionFunction)
  def smallPromotion(salaries: List[Double]): List[Double] =
    promotion(salaries, salary => salary * 1.1)
  def bigPromotion(salaries: List[Double]): List[Double] =
    promotion(salaries, salary => salary * math.log(salary))
  def hugePromotion(salaries: List[Double]): List[Double] =
    promotion(salaries, salary => salary * salary)
}
//
//def urlBuilder(ssl: Boolean, domainName: String): (String, String) => String = {
//  val schema = if (ssl) "https://" else "http://"
//  (endpoint: String, query: String) => s"$schema$domainName/$endpoint?$query"
//}
//val domainName = "www.example.com"
//def getURL = urlBuilder(ssl=true, domainName)
//val endpoint = "users"
//val query = "id=1"
//val url = getURL(endpoint, query) // "https://www.example.com/users?id=1": String
//
//def foldLeft[B](z: B)(op: (B, A) => B): B
//val numbers = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
//val res = numbers.foldLeft(0)((m, n) => m + n)
//print(res) // 55
//
//{
//  val numbers = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
//  val numberFunc = numbers.foldLeft(List[Int]())_
//
//  val squares = numberFunc((xs, x) => xs:+ x*x)
//  print(squares.toString()) // List(1, 4, 9, 16, 25, 36, 49, 64, 81, 100)
//
//  val cubes = numberFunc((xs, x) => xs:+ x*x*x)
//  print(cubes.toString())  // List(1, 8, 27, 64, 125, 216, 343, 512, 729, 1000)
//}
//
//{
//  object Logger {
//    def info(message: String): Unit = println(s"INFO: $message")
//  }
//}

//  import scala.math._
//  case class Circle(radius: Double) {
//    import Circle._
//    def area: Double = calculateArea(radius)
//  }
//  object Circle {
//    private def calculateArea(radius: Double): Double = Pi * pow(radius, 2.0)
//  }
//  val circle1 = new Circle(5.0)
//  circle1.area
//
//object XMLTest1 extends App {
//  val page =
//    <html>
//      <head>
//        <title>Hello XHTML world</title>
//      </head>
//      <body>
//        <h1>Hello world</h1>
//        <p><a href="scala-lang.org">Scala</a> talks XHTML</p>
//      </body>
//    </html>;
//  println(page.toString())
//}
//
//import scala.util.matching.Regex
//
//val numberPattern: Regex = "[0-9]".re
//
//numberPattern.findFirstMatchIn("awesomepassword") match {
//  case Some(_) => println("Password OK")
//  case None => println("Password must contain a number")
//}

import scala.util.Random
object CustomerID {
  def apply(name: String) = s"$name--${Random.nextLong}"
  def unapply(customerID: String): Option[String] = {
    val stringArray: Array[String] = customerID.split("--")
    if (stringArray.tail.nonEmpty) Some(stringArray.head) else None
  }
}
//val customer1ID = CustomerID("Sukyoung")  // Sukyoung--23098234908
//customer1ID match {
//  case CustomerID(name) => println(name)  // prints Sukyoung
//  case _ => println("Could not extract a CustomerID")
//}

trait Buffer {
  type T
  val element: T
}
abstract class SeqBuffer extends Buffer {
  type U
  type T <: Seq[U]
  def length = element.length
}