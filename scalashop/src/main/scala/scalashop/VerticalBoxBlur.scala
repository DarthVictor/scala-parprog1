package scalashop

import java.util.concurrent.ForkJoinTask

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
      var x = from
      while(x < end){
          var y = 0
          while(y < src.height){
              dst.update(x, y, boxBlurKernel(src, x, y, radius))
              y += 1
          }
          x += 1
      }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
      val maxNumOfTasks = Math.min(src.width, numTasks)
      val taskWidth =  src.width / maxNumOfTasks
      var tasks = List[ForkJoinTask[Unit]]()
      for(i <- 0 to maxNumOfTasks - 1){
          tasks = task {
            blur(src, dst, i*taskWidth, (i+1)*taskWidth, radius)
          } :: tasks
      }
      if(taskWidth*maxNumOfTasks < src.width){
          tasks = task {
            blur(src, dst, taskWidth*maxNumOfTasks, src.width, radius)
          } :: tasks
      }
      tasks.map(_.join())
  }

}
