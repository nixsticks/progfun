package funsets

object Main extends App {
//  import FunSets._
//  println(contains(singletonSet(1), 1)

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }

  println(sum(x => x + 0, 1, 3))
}
