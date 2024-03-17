package u02

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test
import task5.Optionals.Optional
import task5.Optionals.Optional.{Empty, Maybe}

import scala.annotation.tailrec

object HelloScala extends App {

  //task 1
  println("task 1")
  println("Hello, Scala!")
  println()

  //task 3
  println("task 3a val lambda")
  val positive: Int => String = _ match
    case x if x >= 0 => "positive"
    case _ => "negative"

  println(positive(5))  // positive
  println(positive(-5)) // negative
  println(positive(0))  // positive
  println()

  println("task 3a method")
  def positiveMethod(x: Int): String = x match
    case x if x >= 0 => "positive"
    case _ => "negative"

  println(positive(5))  // positive
  println(positive(-5)) // negative
  println(positive(0))  // positive
  println()

  println("task 3b val lambda")
  val neg: (String => Boolean) => String => Boolean = predicate => string => !predicate(string)

  val empty: String => Boolean = _ == ""
  val notEmpty = neg(empty)
  println(notEmpty("foo"))   // true
  println(notEmpty(""))      // false
  println(notEmpty("foo") && !notEmpty("")) // true
  println()

  println("task 3b method")
  def negMethod(predicate: String => Boolean): String => Boolean = string => !predicate(string)

  val notEmptyMethod = negMethod(empty)
  println(notEmpty("foo"))   // true
  println(notEmpty(""))      // false
  println(notEmpty("foo") && !notEmpty("")) // true
  println()

  println("task 3c")
  def negGeneric[X](predicate: X => Boolean): X => Boolean = x => !predicate(x)

  val notEmptyGenericNeg = negGeneric(empty)
  println(notEmptyGenericNeg("foo")) // true
  println(notEmptyGenericNeg("")) // false
  println(notEmptyGenericNeg("foo") && !notEmptyGenericNeg("")) // true
  println()

  //task 4
  println("task 4 val lambda curry")
  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z
  println(p1(3)(2)(1)) // false   0 && 0
  println(p1(3)(2)(2)) // false   0 && 1
  println(p1(1)(2)(3)) // false   1 && 0
  println(p1(1)(2)(2)) // true    1 && 1
  println()

  println("task 4 val lambda no curry")
  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z
  println(p2(3, 2, 1)) // false   0 && 0
  println(p2(3, 2, 2)) // false   0 && 1
  println(p2(1, 2, 3)) // false   1 && 0
  println(p2(1, 2, 2)) // true    1 && 1
  println()

  println("task 4 def curry")
  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z
  println(p3(3)(2)(1)) // false   0 && 0
  println(p3(3)(2)(2)) // false   0 && 1
  println(p3(1)(2)(3)) // false   1 && 0
  println(p3(1)(2)(2)) // true    1 && 1
  println()

  println("task 4 def no curry")
  def p4(x: Int, y: Int, z: Int): Boolean = x <= y && y == z
  println(p4(3, 2, 1)) // false   0 && 0
  println(p4(3, 2, 2)) // false   0 && 1
  println(p4(1, 2, 3)) // false   1 && 0
  println(p4(1, 2, 2)) // true    1 && 1
  println()

  //task 5
  println("task 5")
  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))
  println(compose(_ - 1, _ * 2)(5)) // 9
  println(compose(_ - 1, _ * 2)(3)) // 5
  println(compose(_ - 1, _ * 2)(0)) // -1
  println()

  println("task 5 generic")

  def composeGeneric[A,B,C](f: B => C, g: A => B): A => C = x => f(g(x))
  println(composeGeneric[Int, Int, Int](_ - 1, _ * 2)(5)) // 9
  println(composeGeneric[Double, Double, Double](_ - 1, _ * 2)(3)) // 5.0
  println(composeGeneric[Int, Double, Double](_ - 1, _ * 2)(0)) // -1.0
  println()

  //task 6
  println("task 6")

  @tailrec
  //this assumes that a > b
  def gcd(a: Int, b: Int): Int = (a, b) match
    case (a, 0) => a
    case (a, b) => gcd(b, a % b)

  println(gcd(9, 6)) // 3
  println(gcd(15, 5)) // 5
  println(gcd(13, 7)) // 1
  println()

  //task 7
  println("task 7")
  enum Shape:
    case Rectangle(base: Int, height: Int)
    case Circle(radius: Int)
    case Square(side: Int)

  import Shape.*

  object GeometricProperty:
    def perimeter(shape: Shape): Double = shape match
      case Rectangle(base, height) => 2 * (base + height)
      case Circle(radius) => 2 * Math.PI * radius
      case Square(side) => 4 * side

    def scale(shape: Shape, alpha: Double): Double = shape match
      case Rectangle(base, height) => alpha * (base + height)
      case Circle(radius) => alpha * radius
      case Square(side) => 2 * alpha * side

  class GeometricPropertyTest:
    import GeometricProperty.*

    val rectangle: Shape = Rectangle(2, 3)
    val circle: Shape = Circle(2)
    val square: Shape = Square(2)
    val alpha1 = 1.0
    val alpha2 = 2.0
    val alpha5halves = 2.5

    @Test def PerimeterRectangle(): Unit =
      val expected = 10.0
      assertEquals(expected, perimeter(rectangle), 0.0001)

    @Test def PerimeterCircle(): Unit =
      val expected = 12.566370614359172
      assertEquals(expected, perimeter(circle), 0.0001)

    @Test def PerimeterSquare(): Unit =
      val expected = 8.0
      assertEquals(expected, perimeter(square), 0.0001)

    @Test def scaleFactor1Rectangle(): Unit =
      val expected = 5.0
      assertEquals(expected, scale(rectangle, alpha1), 0.0001)

    @Test def scaleFactor1Circle(): Unit =
      val expected = 2.0
      assertEquals(expected, scale(circle, alpha1), 0.0001)

    @Test def scaleFactor1Square(): Unit =
      val expected = 4.0
      assertEquals(expected, scale(square, alpha1), 0.0001)

    @Test def scaleFactor2Rectangle(): Unit =
      val expected = 10.0
      assertEquals(expected, scale(rectangle, alpha2), 0.0001)

    @Test def scaleFactor2Circle(): Unit =
      val expected = 4.0
      assertEquals(expected, scale(circle, alpha2), 0.0001)

    @Test def scaleFactor2Square(): Unit =
      val expected = 8.0
      assertEquals(expected, scale(square, alpha2), 0.0001)

    @Test def scaleFactor5halvedRectangle(): Unit =
      val expected = 12.5
      assertEquals(expected, scale(rectangle, alpha5halves), 0.0001)

    @Test def scaleFactor5halvedCircle(): Unit =
      val expected = 5.0
      assertEquals(expected, scale(circle, alpha5halves), 0.0001)

    @Test def scaleFactor5halvedSquare(): Unit =
      val expected = 10.0
      assertEquals(expected, scale(square, alpha5halves), 0.0001)

  println()

  //task 8
  println("task 8")
  def map[A, B](optional: Optional[A], f: A => B): Optional[B] = optional match
    case Maybe(value) => Maybe(f(value))
    case _ => Empty()

  def filter[A](optional: Optional[A], f: A => Boolean): Optional[A] = optional match
    case Maybe(a) if f(a) => Maybe(a)
    case _ => Empty()

  class OptionalTest:
    @Test def mapShouldReturnTrueWhenConditionIsSatisfied(): Unit =
      val result = map(Maybe(5), _ > 2)
      assertTrue(result == Maybe(true))

    @Test def mapShouldReturnEmptyWhenEmpty(): Unit =
      val result = map(Empty[Int](), _ > 2)
      assertTrue(result == Empty())

    @Test def filterShouldReturnValueWhenConditionIsSatisfied(): Unit =
      val result = filter(Maybe(5), _ > 2)
      assertTrue(result == Maybe(5))

    @Test def filterShouldReturnEmptyWhenConditionIsNotSatisfied(): Unit =
      val result = filter(Maybe(5), _ > 8)
      assertTrue(result == Empty())

    @Test def filterShouldReturnEmptyWhenEmpty(): Unit =
      val result = filter(Empty[Int](), _ > 2)
      assertTrue(result == Empty())
}