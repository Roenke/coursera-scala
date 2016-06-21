package reductions

import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    def check(openCount: Int, ix: Int, until: Int): Boolean = {
      if (openCount < 0 || ix == until) return openCount == 0
      chars(ix) match {
        case '(' => check(openCount + 1, ix + 1, until)
        case ')' => check(openCount - 1, ix + 1, until)
        case _ => check(openCount, ix + 1, until)
      }
    }

    //    check(0, chars.filter(x => x == '(' || x == ')'))
    check(0, 0, chars.length)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, m: Int, b: Int): (Int, Int) = {
      if (idx == until) return (m, b)
      chars(idx) match {
        case '(' => traverse(idx + 1, until, m, b + 1)
        case ')' => traverse(idx + 1, until, math.min(m, b - 1), b - 1)
        case _ => traverse(idx + 1, until, m, b)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      }
      else {
        val mid = from + (until - from) / 2
        val ((ml, bl), (mr, br)) = parallel(reduce(from, mid), reduce(mid, until))
        (math.min(ml, mr + bl), bl + br)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
