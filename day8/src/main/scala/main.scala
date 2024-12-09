import scala.io.Source
import scala.util.{Failure, Success, Using}

@main
def main(): Unit =
  val lines = Using(Source.fromResource("input.txt")) { source =>
    source.getLines().toList
  }

  lines match {
    case Success(value) =>
      println(part1(value))
      println(part2(value))
    case Failure(exception) => println(exception.getMessage)
  }

def part1(input: List[String]): Int = {
  val antennas = input.zipWithIndex.foldLeft(Map.empty[Char, List[Vec]]) {
    case (acc, (line, y)) =>
      line.zipWithIndex.foldLeft(acc) {
        case (innerAcc, (char, x)) if char != '.' =>
          val vec = Vec(x, y)
          innerAcc.updatedWith(char) {
            case Some(value) => Some(vec :: value)
            case None        => Some(List(vec))
          }
        case (innerAcc, _) => innerAcc
      }
  }

  val antinodes = for {
    (_, locations) <- antennas.toSet
    i <- locations
    j <- locations
    if i != j
    antinode = i + ((j - i) * 2)
    if isInBounds(antinode, input)
  } yield antinode

  antinodes.size
}

def part2(input: List[String]): Int = {
  val antennas = input.zipWithIndex.foldLeft(Map.empty[Char, List[Vec]]) {
    case (acc, (line, y)) =>
      line.zipWithIndex.foldLeft(acc) {
        case (innerAcc, (char, x)) if char != '.' =>
          val vec = Vec(x, y)
          innerAcc.updatedWith(char) {
            case Some(value) => Some(vec :: value)
            case None        => Some(List(vec))
          }
        case (innerAcc, _) => innerAcc
      }
  }

  def generateLine(start: Vec, direction: Vec): Iterator[Vec] =
    Iterator
      .iterate(start)(_ + direction)
      .takeWhile(isInBounds(_, input))

  val antinodes = for {
    (_, locations) <- antennas.toSet
    i <- locations
    j <- locations
    if i != j
    distance = j - i
    point <- generateLine(i, distance) ++ generateLine(i, distance * -1)
  } yield point

  antinodes.size
}

private def isInBounds(vec: Vec, input: List[String]): Boolean =
  vec.x >= 0 && vec.x < input.head.length &&
    vec.y >= 0 && vec.y < input.length

final case class Vec(x: Int, y: Int):
  def +(that: Vec): Vec = Vec(x + that.x, y + that.y)

  def -(that: Vec): Vec = Vec(x - that.x, y - that.y)

  def *(scalar: Int): Vec = Vec(x * scalar, y * scalar)
