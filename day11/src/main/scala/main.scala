import scala.io.Source
import scala.util.{Failure, Success, Using}

@main def day10(): Unit =
  val lines = Using(Source.fromResource("input.txt")) { source =>
    source.getLines().toList
  }

  lines match {
    case Success(value) =>
      println(part1(value))
      println(part2(value))
    case Failure(exception) => println(exception.getMessage)
  }

def part1(numbers: List[String]): Long = {
  val initialNumbers =
    """\d+""".r
      .findAllIn(numbers.head)
      .map(_.toLong)
      .toList

  def getNumberOfDigits(n: Long): Int =
    if (n == 0) 1
    else math.floor(math.log10(math.abs(n).toDouble) + 1).toInt

  def transformNumber(n: Long): List[Long] =
    if (n == 0) List(1L)
    else if (getNumberOfDigits(n) % 2 == 0) {
      val numString = n.toString
      val (left, right) = numString.splitAt(numString.length / 2)
      List(left.toLong, right.toLong)
    } else List(n * 2024)

  var currentNumbers = initialNumbers
  for (i <- 0 until 25)
    currentNumbers = currentNumbers.flatMap(transformNumber)

  currentNumbers.length
}

def part2(numbers: List[String]): Long = {
  val initialNumbers =
    """\d+""".r
      .findAllIn(numbers.head)
      .map(_.toLong)
      .toList
      .groupBy(identity)
      .map { case (k, v) => k -> v.size.toLong }

  def getNumberOfDigits(n: Long): Int =
    if (n == 0) 1
    else math.floor(math.log10(math.abs(n).toDouble) + 1).toInt

  var currentNumbers = initialNumbers
  for (i <- 0 until 75) {
    val newNumbers = scala.collection.mutable.Map[Long, Long]()

    for ((n, count) <- currentNumbers) {
      if (n == 0) {
        newNumbers(1L) = newNumbers.getOrElse(1L, 0L) + count
      } else if (getNumberOfDigits(n) % 2 == 0) {
        val numString = n.toString
        val (left, right) = numString.splitAt(numString.length / 2)
        val leftNum = left.toLong
        val rightNum = right.toLong
        newNumbers(leftNum) = newNumbers.getOrElse(leftNum, 0L) + count
        newNumbers(rightNum) = newNumbers.getOrElse(rightNum, 0L) + count
      } else {
        val newNum = n * 2024L
        newNumbers(newNum) = newNumbers.getOrElse(newNum, 0L) + count
      }
    }

    currentNumbers = newNumbers.toMap
  }

  currentNumbers.values.sum
}
