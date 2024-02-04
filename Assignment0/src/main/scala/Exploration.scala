import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Exploration {
  def sum_digits(n: Int): Int = {
    if (n == 0) {
      0
    }else if (n < 0){
      sum_digits(-n)
    }else{
      //println(s"${n%10}, ${n/10}")
      n%10 + sum_digits(n/10)
    }
  }

  def sum_digits_tailrecursive(n: Int): Int = {
    @annotation.tailrec
    def helper(n: Int, sum: Int): Int = {
      if (n == 0) {
        sum
      }else if (n < 0){
        helper(-n, sum)
      }else{
        helper(n/10, sum + n%10)
      }
    }
    helper(n, 0)
  }

  def multiple_choice(c: Char): String = {
    c match{
      case 'A' => "Option 1"
      case 'B' => "Option 2"
      case 'C' => "Option 3"
      case 'D' => "Option 4"
      case _ => "Not a valid option"
    }
  }

  def go = {
    val sumDigits = sum_digits(123456789)
    val sumDigitsTail = sum_digits_tailrecursive(123456789)
    println(s"Sum of digits of 123456789 is $sumDigits, $sumDigitsTail")
    print("Enter an Character: ")
    val choice = readLine().charAt(0)
    println(s"Choice: ${multiple_choice(choice)}")

    val list_one: List[Any] = List(1, 1.0, "one")
    for (n <- list_one){
      println(n)
    }
    val nums: List[Int] = List(0, 1, 2, 3)
    val nums_tripled = nums.map(x => 2*x)
    println(nums_tripled)
    val half_nums = nums.take(2)
    println(half_nums)
    val nums_big = nums_tripled.filter(_ > 3)
    println(nums_big)
    println(nums.exists(n => n > 3))

    val squares_map: Map[Int, Int] = Map(1 -> 1, 2 -> 4, 3 -> 9)
    println(squares_map(1))
    println(squares_map.get(1))
    println(squares_map.contains(4))
    println(squares_map.get(4))
    squares_map.foreach {
      case(n, square) => println(s"The square of ${n} is ${square}")
    }
    val my_map: Map[Int, String] = Map(1 -> "one", 2 -> "two", 3 -> "three")

  }

  def main(args: Array[String]): Unit = {
    go
  }
}
