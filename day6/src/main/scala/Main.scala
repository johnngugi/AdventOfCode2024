import scala.io.Source
import scala.util.{Failure, Success, Using}
import scala.util.control.Breaks._

@main def day6(): Unit =
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

  def turnRight: Directions = this match
    case North => East
    case East => South
    case South => West
    case West => North

extension (p: Point)
  def stepForward(direction: Directions): Point =
    Point(p.x + direction.p.x, p.y + direction.p.y)

case class Grid(data: List[String]):
  private val width: Int = data.head.length
  private val height: Int = data.length

  def isInBounds(p: Point): Boolean =
    p.x >= 0 && p.x < width && p.y >= 0 && p.y < height

  def hasObstacle(p: Point): Boolean =
    isInBounds(p) && data(p.y)(p.x) == '#'

  def findStart: Point =
    val positions = for
      y <- data.indices
      x <- data(y).indices
      if data(y)(x) == '^'
    yield Point(x, y)
    positions.head

case class GuardState(position: Point, direction: Directions, visited: Set[Point])

def part1(numbers: List[String]): Int = {
  val grid = Grid(numbers)
  val initialState = GuardState(grid.findStart, Directions.North, Set(grid.findStart))

  def nextState(state: GuardState): GuardState =
    val nextPosition = state.position.stepForward(state.direction)
    if grid.hasObstacle(nextPosition) then
      state.copy(direction = state.direction.turnRight)
    else if grid.isInBounds(nextPosition) then
      state.copy(
        position = nextPosition,
        visited = state.visited + nextPosition
      )
    else state.copy(nextPosition)

  Iterator
    .iterate(initialState)(nextState)
    .dropWhile(state => grid.isInBounds(state.position))
    .next()
    .visited
    .size
}

def part2(numbers: List[String]): Int = {
  val grid = Grid(numbers)
  val start = grid.findStart
  val initialState = GuardState(start, Directions.North, Set(start))

  def nextState(state: GuardState): GuardState =
    val nextPosition = state.position.stepForward(state.direction)
    if grid.hasObstacle(nextPosition) then
      state.copy(direction = state.direction.turnRight)
    else if grid.isInBounds(nextPosition) then
      state.copy(
        position = nextPosition,
        visited = state.visited + nextPosition
      )
    else state.copy(nextPosition)

  val path = Iterator
    .iterate(initialState)(nextState)
    .dropWhile(state => grid.isInBounds(state.position))
    .next()
    .visited

  val possibleObstacles = path - start

  def checkForLoop(obstaclePos: Point): Boolean = {
    var visited = Set[(Point, Directions)]()
    var currentPosition = start
    var currentDirection = Directions.North

    while (grid.isInBounds(currentPosition)) {
      val state = (currentPosition, currentDirection)
      if (visited.contains(state)) {
        return true
      }

      visited = visited + state
      val nextPosition = currentPosition.stepForward(currentDirection)
      if (nextPosition == obstaclePos || grid.hasObstacle(nextPosition)) {
        currentDirection = currentDirection.turnRight
      } else {
        currentPosition = nextPosition
      }
    }
    false
  }

  possibleObstacles.count(checkForLoop)
}
