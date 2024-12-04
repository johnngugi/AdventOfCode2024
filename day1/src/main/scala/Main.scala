import scala.io.Source
import scala.util.Using
import scala.util.Try
import scala.util.Success
import scala.util.Failure

@main def day1(): Unit =
  part1() match {
    case Success(value)     => println(value)
    case Failure(exception) => println(exception.getMessage)
  }

  part2() match {
    case Success(value)     => println(value)
    case Failure(exception) => println(exception.getMessage)
  }

def part1(): Try[Int] = {
  Using(Source.fromResource("input.txt")) { source =>
    val (left, right) = source
      .getLines()
      .map { l =>
        val a = l.split("\\s+")
        (a(0).toInt, a(1).toInt)
      }
      .toList
      .unzip

    left.sorted
      .zip(right.sorted)
      .map { case (a, b) =>
        (a - b).abs
      }
      .sum()
  }
}

def part2(): Try[Int] = {
  Using(Source.fromResource("input.txt")) { source =>
    val (left, right) = source
      .getLines()
      .map { l =>
        val a = l.split("\\s+")
        (a(0).toInt, a(1).toInt)
      }
      .toList
      .unzip

    val frequencies =
      right.groupBy(identity).map(s => (s._1, s._2.size))

    left.foldLeft(0) { (acc, elem) =>
      acc + elem * frequencies.getOrElse(elem, 0)
    }
  }
}
