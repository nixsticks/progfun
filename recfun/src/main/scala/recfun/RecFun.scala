package recfun

import Array._
import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }


  //  The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it. Write a function that computes the elements of Pascal’s triangle by means of a recursive process.
  //
  //  Do this exercise by implementing the pascal function in Main.scala, which takes a column c and a row r, counting from 0 and returns the number at that spot in the triangle. For example, pascal(0,2)=1,pascal(1,2)=2 and pascal(1,3)=3.

  /**
   * Exercise 1
   */

  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (0, 0) => 1
    case (0, _) => pascal(c, r - 1)
    case (x, y) if x == y => pascal(c - 1, r - 1)
    case _ => pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(c: List[Char], acc: Int = 0): Boolean =
      if (c.isEmpty) acc == 0
      else if (acc < 0) false
      else c.head match {
        case '(' => loop(c.tail, acc + 1)
        case ')' => loop(c.tail, acc - 1)
        case _ => loop(c.tail, acc)
      }

    loop(chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
}
