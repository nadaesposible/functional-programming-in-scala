object Chapter2 {

  def factorial(n: Int): Int = {
    def loop(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else loop(n - 1, n * acc)
    }
    loop(n, 1)
  }

  def fibonacci(n: Int): Int = {
    def loop(n: Int, a: Int, b:Int): Int = {
      if (n == 1) a
      else loop(n - 1, b, a + b)
    }
    loop(n, 0, 1)
  }

  def isSorted[A](xs: Array[A], f: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(pos: Int): Boolean = {
      if (pos >= xs.length - 1) true
      else f(xs(pos), xs(pos + 1)) && loop(pos + 1)
    }
    loop(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a: A => b: B => f(a, b)
  }
}
