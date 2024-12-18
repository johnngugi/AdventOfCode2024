import scala.io.Source
import scala.collection.mutable
import scala.util.{Failure, Success, Using}

@main def day12(): Unit =
  val result = Using(Source.fromResource("input.txt")) { source =>
    val lines = source.getLines().toList
    val grid = Grid(lines)

    val part1Result = grid.getPlants
      .flatMap((plant, points) => grid.findAllRegions(plant, points))
      .map(region => region.area * grid.perimeter(region))
      .sum

    val part2Result = grid.getPlants
      .flatMap((plant, points) => grid.findAllRegions(plant, points))
      .map(region => region.area * grid.sides(region))
      .sum

    (part1Result, part2Result)
  }

  result match
    case Success((part1, part2)) =>
      println(part1)
      println(part2)
    case Failure(exception) =>
      println(s"Error: ${exception.getMessage}")

enum Directions(val p: Point):
  case North extends Directions(Point(0, -1))
  case East extends Directions(Point(1, 0))
  case West extends Directions(Point(-1, 0))
  case South extends Directions(Point(0, 1))

case class Point(x: Int, y: Int):
  def +(other: Point): Point = Point(x + other.x, y + other.y)
  def neighbor(dir: Directions): Point = this + dir.p

case class Region(points: Set[Point], plant: Char):
  def area: Int = points.size

  def contains(p: Point): Boolean = points.contains(p)

case class Grid(data: List[String]):
  private val width: Int = data.head.length
  private val height: Int = data.length

  private def isInBounds(p: Point): Boolean =
    p.x >= 0 && p.x < width && p.y >= 0 && p.y < height

  private def findNeighbours(p: Point): Array[Point] =
    Directions.values.map(p.neighbor).filter(isInBounds)

  def getPlants: Map[Char, Set[Point]] =
    val points = for
      y <- data.indices
      x <- data(y).indices
      char = data(y)(x)
    yield (char, Point(x, y))

    points.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap

  def findAllRegions(plant: Char, points: Set[Point]): List[Region] = {
    val remaining = points.to(mutable.Set)
    val regions = mutable.ListBuffer[Region]()

    while remaining.nonEmpty do
      val start = remaining.head
      val region = mutable.Set[Point]()
      val queue = mutable.Queue[Point](start)

      while queue.nonEmpty do
        val p = queue.dequeue()
        if remaining.remove(p) then
          region.add(p)
          queue.enqueueAll(
            findNeighbours(p).filter(n =>
              remaining.contains(n) && data(n.y)(n.x) == plant
            )
          )

      regions.append(Region(region.toSet, plant))

    regions.toList
  }

  def perimeter(region: Region): Int =
    region.points.iterator.map { p =>
      Directions.values.count { dir =>
        val n = p.neighbor(dir)
        !isInBounds(n) || !region.contains(n)
      }
    }.sum

  def sides(region: Region): Int = {
    def findEdges(pred: Point => Boolean): Set[Point] =
      region.points.filter(pred)

    // Group consecutive points into sides
    def countContiguousGroups(points: Set[Point], byX: Boolean): Int = {
      if points.isEmpty then 0
      else
        val grouped = if byX then points.groupBy(_.y) else points.groupBy(_.x)
        grouped.values.map { pointGroup =>
          val sorted = (if byX then pointGroup.map(_.x)
                        else pointGroup.map(_.y)).toList.sorted
          sorted.tail
            .foldLeft((1, sorted.head)) { case ((count, last), curr) =>
              if curr > last + 1 then (count + 1, curr) else (count, curr)
            }
            ._1
        }.sum
    }

    val edgeTests: List[(Point => Boolean, Boolean)] = List(
      (p => p.y == 0 || !region.contains(p.copy(y = p.y - 1)), true), // top
      (
        p => p.y == height - 1 || !region.contains(p.copy(y = p.y + 1)),
        true
      ), // bottom
      (p => p.x == 0 || !region.contains(p.copy(x = p.x - 1)), false), // left
      (
        p => p.x == width - 1 || !region.contains(p.copy(x = p.x + 1)),
        false
      ) // right
    )
    // Count sides in each direction
    edgeTests.map { case (pred, byX) =>
      countContiguousGroups(findEdges(pred), byX)
    }.sum
  }
