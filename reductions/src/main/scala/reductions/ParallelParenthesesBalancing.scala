package reductions

import scala.annotation._
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
  ) withWarmer(new Warmer.Default)

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
    var i = 0
    var balance = 0
    while(i < chars.length){
      val char = chars(i)
      if(char == '(') balance += 1
      else if(char == ')') {
        balance -= 1
        if(balance < 0) return false
      }
      i += 1
    }
    balance == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

//    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {
//      ???
//    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if(until - from < threshold || until - from < 2){
          var i = from
          var balance = 0
          var min = 0
          while(i < until){
              val char = chars(i)
              if(char == '(') balance += 1
              else if(char == ')') {
                balance -= 1
                if(balance < min) min = balance
              }
              i += 1
          }
          (balance, min)
      }
      else{
          val middle = (from + until) / 2
          val (left, right) = parallel(reduce(from, middle), reduce(middle, until))
        //println(chars.toString, from, until)
        //println(left, right)
          val min = math.min(left._2, left._1 + right._2)
          val balance = left._1 + right._1
          (balance, min)
      }
    }

    val res = reduce(0, chars.length)
    res._1 == 0 && res._2 >= 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
