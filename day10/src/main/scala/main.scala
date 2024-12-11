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

case class Point(x: Int, y: Int)

enum Directions(val p: Point):
  case North extends Directions(Point(0, -1))
  private case East extends Directions(Point(1, 0))
  private case West extends Directions(Point(-1, 0))
  private case South extends Directions(Point(0, 1))

case class Grid(data: List[String]):
  private val width: Int = data.head.length
  private val height: Int = data.length

  private def isInBounds(p: Point): Boolean =
    p.x >= 0 && p.x < width && p.y >= 0 && p.y < height

  private def findNeighbours(p: Point): Array[Point] =
    Directions.values
      .map(direction => Point(p.x + direction.p.x, p.y + direction.p.y))
      .filter(isInBounds)

  def findZeros: IndexedSeq[Point] =
    for
      y <- data.indices
      x <- data(y).indices
      if data(y)(x) == '0'
    yield Point(x, y)

  def findScore(p: Point): Set[Point] =
    if data(p.y)(p.x).asDigit == 9 then Set(p)
    else
      findNeighbours(p)
        .filter { neighbour =>
          val neighbourP = data(neighbour.y)(neighbour.x).asDigit
          neighbourP == data(p.y)(p.x).asDigit + 1
        }
        .toSet
        .flatMap(findScore)

  def findRating(p: Point): Int =
    if data(p.y)(p.x).asDigit == 9 then 1
    else
      findNeighbours(p)
        .filter { neighbour =>
          val neighbourP = data(neighbour.y)(neighbour.x).asDigit
          neighbourP == data(p.y)(p.x).asDigit + 1
        }
        .map(findRating)
        .sum

def part1(numbers: List[String]): Int = {
  val grid = Grid(numbers)

  grid.findZeros.map(grid.findScore).map(_.size).sum
}

def part2(numbers: List[String]): Int = {
  val grid = Grid(numbers)

  grid.findZeros.map(grid.findRating).sum
}
