import scala.util.Using
import scala.util.Success
import scala.util.Failure
import scala.io.Source
import scala.util.Try

case class Rule(from: Long, to: Long)

@main def day5(): Unit =
  processInput("input.txt") match {
    case Success((rules, numbers)) =>
      println(s"Part 1 result: ${part1(rules, numbers)}")
      println(s"Part 2 result: ${part2(rules, numbers)}")
    case Failure(exception) =>
      println(s"Error processing input: ${exception.getMessage}")
  }

def processInput(filename: String): Try[(Set[Rule], List[String])] = {
  Using(Source.fromResource(filename)) { source =>
    val lines = source.getLines().toList
    val (ruleStrings, numberStrings) = lines.span(_.nonEmpty)
    val rules = ruleStrings.map(parseRule).toSet
    (rules, numberStrings.tail) // Skip the empty line
  }
}

def parseRule(line: String): Rule = line match {
  case s"$from|$to" => Rule(from.toLong, to.toLong)
  case _ => throw new IllegalArgumentException(s"Invalid rule format: $line")
}

def part1(rules: Set[Rule], numbers: List[String]): Long = {
  def isValidSequence(sequence: Array[Long]): Boolean = {
    val pairs = for {
      i <- sequence.indices
      j <- i + 1 until sequence.length
    } yield !rules.contains(Rule(sequence(j), sequence(i)))

    pairs.forall(identity)
  }

  numbers.map { pages =>
    val sequence = pages.split(',').map(_.toLong)
    if (isValidSequence(sequence)) sequence(sequence.length / 2) else 0
  }.sum
}

def part2(rules: Set[Rule], numbers: List[String]): Long = {
  def isValidSequence(sequence: Array[Long]): Boolean = {
    val pairs = for {
      i <- sequence.indices
      j <- i + 1 until sequence.length
    } yield !rules.contains(Rule(sequence(j), sequence(i)))

    pairs.forall(identity)
  }

  def sortSequence(sequence: Array[Long]): Array[Long] = {
    while (!isValidSequence(sequence)) {
      for {
        i <- sequence.indices
        j <- i + 1 until sequence.length
        if rules.contains(Rule(sequence(j), sequence(i)))
      } {
        // Swap elements
        val temp = sequence(j)
        sequence(j) = sequence(i)
        sequence(i) = temp
      }
    }
    sequence
  }

  numbers.map { pages =>
    val sequence = pages.split(',').map(_.toLong)
    if (isValidSequence(sequence)) 0
    else sortSequence(sequence)(sequence.length / 2)
  }.sum
}
