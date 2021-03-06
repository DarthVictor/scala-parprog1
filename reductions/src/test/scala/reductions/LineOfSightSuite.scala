package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }

  test("upsweep should correctly compute the tree on the indices 1 until 5 of a 5 element array for threshold 1") {
    val res = upsweep(Array[Float](0f, 1f, 8f, 9f, 10f), 1, 5, 1)
    assert(4f == 4f)
  }

  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("lineOfSight should correctly handle an array with only one element (which corresponds to the observer)") {
    val output = new Array[Float](1)
    lineOfSight(Array[Float](0f), output)
    assert(output.toList == List(0f))
  }

  test("parLineOfSight should correctly handle an array of size 5") {
    val output = new Array[Float](5)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f, 20f), output, 1)
    assert(output.toList == List(0f, 1f, 4f, 4f, 5f))
  }

}

