import scala.io.Source
import scala.util.{Failure, Success, Using}

@main
def main(): Unit =
  val lines = Using(Source.fromResource("input.txt")) { source =>
    source
      .getLines()
      .map { l =>
        l.split("\\s+").map(_.toInt)
      }
      .toList
  }

  lines match {
    case Success(value) =>
      println(part1(value))
      println(part2(value))
    case Failure(exception) => println(exception.getMessage)
  }

def isLineSafe(numbers: Array[Int]): Boolean =
  val isIncreasing = numbers.sliding(2).forall { case Array(a, b) =>
    a <= b && (a - b).abs <= 3 && (a - b).abs >= 1
  }

  val isDecreasing = numbers.sliding(2).forall { case Array(a, b) =>
    a >= b && (a - b).abs <= 3 && (a - b).abs >= 1
  }

  isIncreasing || isDecreasing

def part1(lines: List[Array[Int]]): Int =
  lines.count(isLineSafe)

def part2(lines: List[Array[Int]]): Int =
  lines.count { numbers =>
    numbers.zipWithIndex.exists { num =>
      val skipped = numbers.patch(num._2, Nil, 1)
      isLineSafe(skipped)
    }
  }
