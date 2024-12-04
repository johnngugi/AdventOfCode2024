import scala.io.Source
import scala.util.{Failure, Success, Using}

@main def day3(): Unit =
  val lines = Using(Source.fromResource("input.txt")) { source =>
    source
      .getLines()
      .toList
  }

  lines match {
    case Success(value) =>
      println(part1(value))
      println(part2(value.mkString))
    case Failure(exception) => println(exception.getMessage)
  }

def part1(numbers: List[String]): Int =
  val mulPattern = """mul\((\d{1,3}),(\d{1,3})\)""".r
  numbers
    .map(mulPattern.findAllMatchIn)
    .flatMap { matches =>
      matches.map { a =>
        a.group(1).toInt * a.group(2).toInt
      }
    }
    .sum

case class State(sum: Int = 0, doing: Boolean = true)

def part2(numbers: String): Int =
  """mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)""".r
    .findAllIn(numbers)
    .foldLeft(State())({
      case (State(soFar, true), s"mul($x,$y)") =>
        State(sum = soFar + x.toInt * y.toInt)
      case (s, "don't()") => s.copy(doing = false)
      case (s, "do()")    => s.copy(doing = true)
      case (s, _)         => s
    })
    .sum
