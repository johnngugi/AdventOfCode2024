import scala.io.Source
import scala.util.{Failure, Success, Using}

@main def day4(): Unit =
  val lines = Using(Source.fromResource("input.txt")) { source =>
    source
      .getLines()
      .toList
  }

  lines match {
    case Success(value) =>
      println(part1(value))
      println(part2(value))
    case Failure(exception) => println(exception.getMessage)
  }

case class Point(x: Int, y: Int)

def part1(numbers: List[String]): Int = {
  val height = numbers.size
  val width = numbers.head.length

  def getWord(start: Point, dx: Int, dy: Int): Option[String] = {
    if (
      start.x + 3 * dx < 0 || start.x + 3 * dx >= width ||
      start.y + 3 * dy < 0 || start.y + 3 * dy >= height
    ) return None

    Some(
      (0 to 3).map(i => numbers(start.y + i * dy)(start.x + i * dx)).mkString
    )
  }

  val count = for {
    y <- 0 until height
    x <- 0 until width
    direction <- List(
      (1, 0), // right
      (-1, 0), // left
      (0, 1), // down
      (0, -1), // up
      (1, 1), // diagonal down-right
      (-1, -1), // diagonal up-left
      (1, -1), // diagonal up-right
      (-1, 1) // diagonal down-left
    )
    word <- getWord(Point(x, y), direction._1, direction._2)
    if word == "XMAS"
  } yield 1

  count.sum
}

def part2(numbers: List[String]): Int = {
  val height = numbers.size
  val width = numbers.head.length

  val crosses = for {
    y <- 0 until height
    x <- 0 until width
    if isXmasCross(Point(x, y), numbers)
  } yield Point(x, y)

  crosses.size
}

def isXmasCross(point: Point, numbers: List[String]): Boolean = {
  def isInBounds(p: Point): Boolean = {
    p.x >= 0 && p.x < numbers.head.length &&
    p.y >= 0 && p.y < numbers.size
  }

  def getDiagonalString(start: Point, dx: Int, dy: Int): String = {
    val p1 = Point(start.x - dx, start.y - dy)
    val p2 = start
    val p3 = Point(start.x + dx, start.y + dy)

    if (!isInBounds(p1) || !isInBounds(p2) || !isInBounds(p3)) return ""

    s"${numbers(p1.y)(p1.x)}${numbers(p2.y)(p2.x)}${numbers(p3.y)(p3.x)}"
  }

  if (numbers(point.y)(point.x) != 'A') return false

  // Check both diagonals
  val diagonal1 = getDiagonalString(point, 1, 1) // top-left to bottom-right
  val diagonal2 = getDiagonalString(point, 1, -1) // bottom-left to top-right

  val valid = Set("MAS", "SAM")
  (valid.contains(diagonal1) || valid.contains(diagonal1.reverse)) &&
  (valid.contains(diagonal2) || valid.contains(diagonal2.reverse))
}
