object assignment01 {

  type F = Double => Double  // a continuous function of one variable defined over real numbers
  type R = (Double, Double)  // a range over which to search for a root
  type G = (R,F) => Double   // a strategy for approximating a root using bracketed root finding
  type A = F => R => Double  // a function and a range, which yields a root
  
  def main(args: Array[String]) = {
    val x1 = 1071
    val x2 = 462
    val expected_gcd = 21
    val x = 2.0
    val eps = 0.000001
    val max_iterations = 1000000
    val low_guess = 0.5
    val high_guess = 10.0
    val r1 = (low_guess, high_guess)
    val f1: F = x => x * x - 2   // square root of 2 expressed so that root finding will solve it
    val f2: F = x => 3 * x * x * x - 4 * x * x + 12 * x - 37  // a third degree polynomial to test root finding
    val expected_root_of_2 = 1.4142
    val expected_root_polynomial = 2.1465
    val root_bisection: A = root_find_bisection(eps)(max_iterations)
    val root_regula: A = root_find_regula_falsi(eps)(max_iterations)
    val g_bisection: G = (r, f) => r._1 + (r._2 - r._1)/2 // For task 7
    val g_regula: G = (r, f) => ((r._1 * f(r._2)) - (r._2 * f(r._1))) / (f(r._2) - f(r._1)) // For task 7
    val root_bisection_general: A = root_find(eps)(max_iterations)(g_bisection)
    val root_regula_general: A = root_find(eps)(max_iterations)(g_regula)

    // 1. Imperative version of Euclid's GCD algorithm
    val actual_1 = gcd_imperative(x1, x2)
    val error_1 = Math.abs(actual_1 - expected_gcd)
    println(s"The GCD of $x1 and $x2 is ${actual_1}, error is ${error_1}")

    // 2. Fully functional version of Euclid's GCD algorithm
    val actual_2 = gcd_functional(x1, x2)
    val error_2 = Math.abs(actual_2 - expected_gcd)
    println(s"The GCD of $x1 and $x2 is ${actual_2}, error is ${error_2}")

    // 3. Square root of 2 using the bisection method, where the function and the bracketing mechanism are specific to the function used
    val actual_3 = square_root_bisection(x, eps, max_iterations, low_guess, high_guess)
    val error_3 = Math.abs(actual_3 - expected_root_of_2)
    println(s"Using bisection, square root of ${x} is ${actual_3}, error is ${error_3}")

    // 4. Square root of 2 using the Regula Falsi method, where the function and the bracketing mechanism are specific to the function used
    val actual_4 = square_root_regula_falsi(x, eps, max_iterations, low_guess, high_guess)
    val error_4 = Math.abs(actual_4 - expected_root_of_2)
    println(s"Using regula falsi, square root of ${x} is ${actual_4}, error is ${error_4}")

    // 5. Square root of 2 using the bisection method, where the function to solve is passed in but the bracketing mechanism is specific
    val actual_5 = root_bisection(f1)(r1)
    val error_5 = Math.abs(actual_5 - expected_root_of_2)
    println(s"Using bisection for any root finding method, square root of ${x} is ${actual_5}, error is ${error_5}")

    // 6. Square root of 2 using the Regula Falsi method, where the function to solve is passed in but the bracketing mechanism is specific
    val actual_6 = root_regula(f1)(r1)
    val error_6 = Math.abs(actual_6 - expected_root_of_2)
    println(s"Using regula falsi for any root finding method, square root of ${x} is ${actual_6}, error is ${error_6}")

    // 7. Square root of 2 using bisection, where both the function and the bracketing mechanism are passed in
    val actual_7 = root_bisection_general(f1)(r1)
    val error_7 = Math.abs(actual_7 - expected_root_of_2)
    println(s"Using generic root finding with bisection supplied, square root of ${x} is ${actual_7}, error is ${error_7}")

    // 8. Square root of 2 using the Regula Falsi method, where both the function and the bracketing mechanism are passed in
    val actual_8 = root_regula_general(f1)(r1)
    val error_8 = Math.abs(actual_8 - expected_root_of_2)
    println(s"Using generic root finding with regula falsi supplied, square root of ${x} is ${actual_8}, error is ${error_8}")

    // 9. Root of 3x^3-4x^2+12x-37 using bisection, where both the function and the bracketing mechanism are passed in
    val actual_9 = root_bisection_general(f2)(r1)
    val error_9 = Math.abs(actual_9 - expected_root_polynomial)
    println(s"Using generic root finding with bisection supplied, a root of y=3x^3-4x^2+12x-37 is ${actual_9}, error is ${error_9}")

    // 10. Root of 3x^3-4x^2+12x-37 using Regula Falsi, where both the function and the bracketing mechanism are passed in
    val actual_10 = root_regula_general(f2)(r1)
    val error_10 = Math.abs(actual_10 - expected_root_polynomial)
    println(s"Using generic root finding with regula falsi supplied, a root of y=3x^3-4x^2+12x-37 is ${actual_10}, error is ${error_10}")

  }
 
  // Task 1 - Euclid's GCD algorithm using imperative programming
  def gcd_imperative(x: Int, y: Int): Int = {
    var xx = x
    var yy = y
    var t = y
    while (yy != 0){
      t = yy
      yy = xx%yy
      xx = t
    }
    xx
  }

  // Task 2 - Euclid's GCD algorithm using functional programming
  def gcd_functional(x: Int, y: Int): Int = {
    def helper(x: Int, y: Int): Int = {
      if (y == 0) x
      else helper(y, x%y)
    }
    helper(x, y)
  }

  // Task 3 - Square root of 2 using bisection, both the function to solve and the bracketing mechanism are baked into the implementation
  // https://en.wikipedia.org/wiki/Bisection_method
  def square_root_bisection(x: Double, eps: Double, max_iterations: Int, low_guess: Double, high_guess: Double): Double = {
    val funct: F = input => input * input - x

    def helper(eps: Double, remaining_iters: Int, f: F, low_guess: Double, high_guess: Double): Double = {
      val mid = low_guess + (high_guess - low_guess)/2 //guess
      //stop if no more iterations, root found, or within epsilon -> return guess
      if (remaining_iters == 0 || f(mid) == 0 || (high_guess - low_guess) < eps) mid
      //mid and low evaluate to same sign -> replace low with mid
      else if ((f(mid) < 0) == (f(low_guess) < 0)) helper(eps, remaining_iters-1, f, mid, high_guess)
      //mid and high evaluate to same sign -> replace high with mid
      else helper(eps, remaining_iters-1, f, low_guess, mid)
    }
    
    helper(eps, max_iterations, funct, low_guess, high_guess)
  }

  // Task 4 - Square root of 2 using bisection, both the function to solve and the bracketing mechanism are baked into the implementation
  // https://en.wikipedia.org/wiki/Regula_falsi
  def square_root_regula_falsi(x: Double, eps: Double, max_iterations: Int, low_guess: Double, high_guess: Double): Double = {
    val funct: F = input => input * input - x

    def helper(eps: Double, remaining_iters: Int, f:F, low_guess: Double, high_guess: Double): Double = {
      val f_of_low = f(low_guess)
      val f_of_high = f(high_guess)
      val intercept = ((low_guess * f_of_high) - (high_guess * f_of_low)) / (f_of_high - f_of_low) //guess
      //stop if no more iterations, root found, or within epsilon -> return guess
      if (remaining_iters == 0 || f(intercept) == 0 || (high_guess - low_guess) < eps) intercept
      //intercept and low evaluate to same sign -> replace low with intercept
      else if ((f(intercept) < 0) == (f_of_low < 0)) helper(eps, remaining_iters-1, f, intercept, high_guess)
      //intercept and high evaluate to same sign -> replace high with intercept
      else helper(eps, remaining_iters-1, f, low_guess, intercept)
    }

    helper(eps, max_iterations, funct, low_guess, high_guess)
  }

  // Task 5 - Root finder using bisection. It accepts any function to solve, but the bracketing mechanism is baked into the implementation
  def root_find_bisection(eps: Double)(max_iterations: Int)(f: F)(r: R): Double = {
    def helper(eps: Double, remaining_iters: Int, f: F, low: Double, high: Double): Double = {
      val mid = low + (high - low)/2
      if (remaining_iters == 0 || f(mid) == 0 || (high - low) < eps) mid
      else if ((f(mid) < 0) == (f(low) < 0)) helper(eps, remaining_iters-1, f, mid, high)
      else helper(eps, remaining_iters-1, f, low, mid)
    }

    helper(eps, max_iterations, f, r._1, r._2)
  }

  // Task 6 - Root finder using Regula Falsi. It accepts any function to solve, but the bracketing mechanism is baked into the implementation
  def root_find_regula_falsi(eps: Double)(max_iterations: Int)(f: F)(r: R): Double = {
    def helper(eps: Double, remaining_iters: Int, f: F, low: Double, high: Double): Double = {
      val f_of_low = f(low)
      val f_of_high = f(high)
      val intercept = ((low * f_of_high) - (high * f_of_low)) / (f_of_high - f_of_low)
      if (remaining_iters == 0 || f(intercept) == 0 || (high - low) < eps) intercept
      else if ((f(intercept) < 0) == (f_of_low < 0)) helper(eps, remaining_iters-1, f, intercept, high)
      else helper(eps, remaining_iters-1, f, low, intercept)
    }

    helper(eps, max_iterations, f, r._1, r._2)
  }

  // Task 7 - Root finder using any bracketing mechanism and which accepts any function to solve.
  def root_find(eps: Double)(max_iterations: Int)(g: G)(f: F)(r: R): Double = {
    def helper(eps: Double, remaining_iters: Int, g: G, f: F, r: R): Double = {
      val c = g(r, f)
      if (remaining_iters == 0 || f(c) == 0 || (r._2 - r._1) < eps) c
      else if ((f(c) < 0) == (f(r._1) < 0)) helper(eps, remaining_iters-1, g, f, (c, r._2))
      else helper(eps, remaining_iters-1, g, f, (r._1, c))
    }

    helper(eps, max_iterations, g, f, r)
  }

}
