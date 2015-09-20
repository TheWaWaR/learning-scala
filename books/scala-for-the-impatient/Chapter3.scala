
import scala.util.Random
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.{Buffer, ArrayBuffer}
import java.util.TimeZone
import java.awt.datatransfer._

object Chapter3 extends App {
  /* 1. Write a code snippet that sets a to an array of n random integers
   between 0 (inclusive) and n (exclusive). */
  def randomN(n: Int) : Array[Int] = {
    val arr = new Array[Int](n)
    for (i <- 0 until n) {
      arr(i) = Random.nextInt(n)
    }
    arr
  }

  for (n <- List(3, 7, 11)) {
    println("Case 1:", n, randomN(n).toList)
  }

  /* 2. Write a loop that swaps adjacent elements of an array of integers.
   For example, Array(1, 2, 3, 4, 5) becomes Array(2, 1, 4, 3, 5). */
  def case2(src: Array[Int], target: Array[Int]) {
    for (i <- 0 until src.length / 2) {
      val n = i * 2
      val t = src(n)
      src(n) = src(n+1)
      src(n+1) = t
    }
    for (i <- 0 until src.length) {
      assert(src(i) == target(i), "Error result: %s".format(src.toList))
    }
    println("Case 2:", src.toList)
  }

  for ((src, target) <- Array(
    (Array(1, 2, 3, 4, 5), Array(2, 1, 4, 3, 5)),
    (Array(1), Array(1)),
    (Array(1, 2), Array(2, 1)),
    (Array(1, 2, 3, 4), Array(2, 1, 4, 3)))) {
    case2(src, target)
  }

  /* 3. Repeat the preceding assignment, but produce a new array with
   the swapped values. Use for/yield. */
  def case3(src: Array[Int], target: Array[Int]) {
    val newArr = for (i <- 0 until src.length) yield {
      if (i % 2 == 0 && i == src.length - 1) {
        src(i)
      } else if (i % 2 == 0) {
        src(i + 1)
      } else {
        src(i - 1)
      }
    }

    for (i <- 0 until src.length) {
      assert(newArr(i) == target(i), "Error result: %s".format(newArr.toList))
    }
    println("Case 3:", newArr.toList)
  }

  for ((src, target) <- Array(
    (Array(1, 2, 3, 4, 5), Array(2, 1, 4, 3, 5)),
    (Array(1), Array(1)),
    (Array(1, 2), Array(2, 1)),
    (Array(1, 2, 3, 4), Array(2, 1, 4, 3)))) {
    case3(src, target)
  }

  /* 4. Given an array of integers, produce a new array that contains
   all positive values of the original array, in their original order,
   followed by all values that are zero or negative, in their original
   order.*/
  def case4(src: Array[Int]): Array[Int] = {
    val neg = for (i <- 0 until src.length if src(i) > 0) yield src(i)
    val pos = for (i <- 0 until src.length if src(i) <= 0) yield src(i)
    (neg ++ pos).toArray
  }

  for ((src, target) <- Array(
    (Array(1, 3, -2, 4, -3, -5), Array(1, 3, 4, -2, -3, -5)),
    (Array(1, 3, -2, 4, -3, 123), Array(1, 3, 4, 123, -2, -3)),
    (Array(-1, 3, 0, 4, -3, 123), Array(3, 4, 123, -1, 0, -3)),
    (Array(-1, 3, -2, 4, -3, -123), Array(3, 4, -1, -2, -3, -123))
  )) {
    val res = case4(src)
    assert(res.length == target.length, "Error result: %s, %s".format(res.toList, target.toList))
    for (i <- 0 until res.length) {
      assert(res(i) == target(i), "Error result: %s, %s".format(res.toList, target.toList))
    }
    println("Case 4:", res.toList)
  }

  /* 5. How do you compute the average of an `Array[Double]`? */
  def case5(nums: Array[Double]): Double = {
    nums.sum / nums.length
  }

  val nums = Array(1.2, 2.2, 3.2, 4.2)
  println("Case 5:", nums.toList, " ==> ", case5(nums))


  /* 6. How do you rearrange the elements of an `Array[Int]` so that they
   appear in reverse sorted order? How do you do the same with an `ArrayBuffer[Int]` ? */
  val arr: Array[Int] = Array(1, 2, 3, 4, 5)
  val arrBuf = ArrayBuffer[Int](1, 2, 3, 4, 5)
  println("Case 6:", arr.reverse.toList)
  println("Case 6:", arrBuf.reverse.toList)

  /* 7. Write a code snippet that produces all values from an array
   with duplicates removed. (Hint: Look at Scaladoc.) */
  println("Case 7:", Array(1, 2, 2, 3, 4, 4, 4, 4, 5, 5).distinct.toList)

  /* 8. Rewrite the example at the end of Section 3.4, "Transforming Arrays" on page 32.
   Collect indexes of the negative elements, reverse the sequence, drop the last index,
   and call `a.remove(i)` for each index. Compare the efficiency of this approach
   with the two approaches in Section 3.4. */
  // !!! [ignore]

  /* 9. Make a collection of all time zones returned by `java.util.TimeZone.getAvailableIDs`
   that are in America. Strip off the "America/" prefix and sort the result. */
  println("Case 9:", (for (item <- TimeZone.getAvailableIDs
    if item.startsWith("America/")) yield item).sorted.toList)


  /* 10. Import `java.awt.datatransfer._` and make an object of type SystemFlavorMap with the call
   `val flavors = SystemFlavorMap.getDefaultFlavorMap().asInstanceOf[SystemFlavorMap]`
   Then call the `getNativesForFlavor` method with parameter `DataFlavor.imageFlavor`
   and get the return value as a Scala buffer.
   (Why this obscure class? It’s hard to find uses of `java.util.List` in the standard Java library.)  */
  val flavors = SystemFlavorMap.getDefaultFlavorMap().asInstanceOf[SystemFlavorMap]
  val imgFlavors: Buffer[String] = flavors.getNativesForFlavor(DataFlavor.imageFlavor)
  println("Case 10:", imgFlavors)
}

Chapter3.main(null)
