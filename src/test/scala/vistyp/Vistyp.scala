package vistyp

trait Monoid[A]:
  def combine(x: A, y: A): A
  def empty: A

object Monoid:
  def apply[A](using monoid: Monoid[A]): Monoid[A] = monoid

given Monoid[Int] with
  def combine(x: Int, y: Int): Int = x + y
  def empty: Int = 0

def sum[A: Monoid](xs: List[A]): A =
  xs.foldLeft(Monoid[A].empty)(Monoid[A].combine)

def testSum(): Unit =
  val result = sum(List(1, 2, 3))
  assert(result == 6)

class MonoidTest extends munit.FunSuite:
  test("sum") {
    testSum()
  }
end MonoidTest
