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

def part1(numbers: List[String]): Long = {
  numbers
    .map { line =>
      """\d+""".r.findAllIn(line).map(_.toLong).toList
    }
    .filter { line =>
      val total = line.head
      val first = line(1)
      val rest = line.drop(2)
      val possibleCombinations = Math.pow(2, rest.length).toLong

      (0L until possibleCombinations).exists { i =>
        var result = first
        for (j <- rest.indices) {
          val shouldMultiply = (i & (1 << j)) > 0
          if (shouldMultiply) {
            result = result * rest(j)
          } else {
            result = result + rest(j)
          }
        }
        result == total
      }
    }
    .map(_.head)
    .sum
}

def part2(numbers: List[String]): Long = {
  def canMakeValue(numbers: List[Long], target: Long): Boolean = {
    val minPossible = numbers.sum
    val maxPossible = numbers.product
    if (target < minPossible && target > maxPossible) return false

    val operatorCount = numbers.length - 1
    val combinations = math.pow(3, operatorCount).toLong

    (0L until combinations).exists { pattern =>
      var result = numbers.head
      for (i <- numbers.tail.indices) {
        val possible = (pattern / math.pow(3, i).toLong) % 3

        possible match {
          case 0 => result = result + numbers.tail(i)
          case 1 => result = result * numbers.tail(i)
          case 2 =>
            result = (result.toString + numbers.tail(i).toString).toLong
        }
      }
      result == target
    }

  }

  numbers
    .map { line =>
      """\d+""".r.findAllIn(line).map(_.toLong).toList
    }
    .filter { line =>
      val target = line.head
      val operands = line.tail

      canMakeValue(operands, target)
    }
    .map(_.head)
    .sum
}
