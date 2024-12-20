import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

case class Point(x: Int, y: Int):
  def +(other: Point): Point = Point(x + other.x, y + other.y)

case class Robot(position: Point, velocity: Point)

@main def day14(): Unit = {
  val pattern = """(-?\d+)""".r

  def processInput(filename: String): Try[List[Robot]] = {
    Using(Source.fromResource(filename)) { source =>
      val points = source.getLines().toList.map { line =>
        pattern.findAllIn(line).map(_.toInt).toList
      }

      points.map { point =>
        Robot(
          Point(point(0), point(1)),
          Point(point(2), point(3))
        )
      }
    }
  }

  processInput("input.txt") match {
    case Success(machines) =>
      println(s"Part 1: ${part1(machines)}")
      println(s"Part 2: ${part2(machines)}")
    case Failure(exception) =>
      System.err.println(s"Error processing input: ${exception.getMessage}")
      System.exit(1)
  }
}

def calculateNewPosition(
    robot: Robot,
    n: Int,
    gridWidth: Int,
    gridHeight: Int
): Robot = {
  // Calculate raw new positions as Long to avoid integer overflow
  val rawX = robot.position.x.toLong + robot.velocity.x.toLong * n
  val rawY = robot.position.y.toLong + robot.velocity.y.toLong * n

  // Now we can use floorMod since we're working with Long values
  val wrappedX = math.floorMod(rawX, gridWidth.toLong)
  val wrappedY = math.floorMod(rawY, gridHeight.toLong)

  robot.copy(position = Point(wrappedX.toInt, wrappedY.toInt))
}

def part1(robots: List[Robot]): Long = {
  val gridWidth = 101
  val gridHeight = 103
  val seconds = 100

  val newPositions =
    robots.map(calculateNewPosition(_, seconds, gridWidth, gridHeight))

  val halfX = gridWidth / 2
  val halfY = gridHeight / 2

  def getQuadrant(position: Point): Option[Int] = {
    if (position.x == halfX || position.y == halfY) None
    else
      Some(
        (if (position.x > halfX) 1 else 0) +
          (if (position.y > halfY) 2 else 0)
      )
  }

  val quadrantCounts =
    newPositions
      .map(_.position)
      .flatMap(getQuadrant)
      .groupMapReduce(identity)(_ => 1)(_ + _)

  (0 to 3).map(quadrantCounts.getOrElse(_, 0)).product
}

def part2(robots: List[Robot]): Long = {
  val gridWidth = 101
  val gridHeight = 103

  def getEasterEgg(i: Int): Int = {
    val newPositions = robots
      .map(calculateNewPosition(_, i, gridWidth, gridHeight))
      .map(_.position)
      .toSet

    if newPositions.exists(p =>
        (0 until 10).forall(i => newPositions.contains(Point(p.x + i, p.y)))
      )
    then i
    else getEasterEgg(i + 1)
  }

  getEasterEgg(0)
}
