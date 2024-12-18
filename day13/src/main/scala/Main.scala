import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

case class Equation(a: Double, b: Double, c: Double)
case class Coordinates(x: Int, y: Int)
case class Machine(
    buttonA: Coordinates,
    buttonB: Coordinates,
    prize: Coordinates
)

@main def day13(): Unit = {
  val ButtonPattern = """Button ([AB]): X\+(\d+), Y\+(\d+)""".r
  val PrizePattern = """Prize: X=(\d+), Y=(\d+)""".r

  def parseSection(lines: List[String]): Option[Machine] = {
    val coordinates = lines.foldLeft(Map.empty[String, Coordinates]) {
      case (acc, ButtonPattern(button, x, y)) =>
        acc + (s"Button$button" -> Coordinates(x.toInt, y.toInt))
      case (acc, PrizePattern(x, y)) =>
        acc + ("Prize" -> Coordinates(x.toInt, y.toInt))
      case (acc, _) => acc
    }

    for {
      buttonA <- coordinates.get("ButtonA")
      buttonB <- coordinates.get("ButtonB")
      prize <- coordinates.get("Prize")
    } yield Machine(buttonA, buttonB, prize)
  }

  def processInput(filename: String): Try[List[Machine]] = {
    Using(Source.fromResource(filename)) { source =>
      val sections = source
        .getLines()
        .foldLeft(List(List.empty[String])) { (acc, line) =>
          if (line.trim.isEmpty) List.empty[String] :: acc
          else (line :: acc.head) :: acc.tail
        }
        .map(_.reverse)
        .reverse
        .filter(_.nonEmpty)

      sections.flatMap(parseSection)
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

def solve(eq1: Equation, eq2: Equation): Option[(Double, Double)] = {
  // Using Cramer's rule to solve the system
  val determinant = eq1.a * eq2.b - eq1.b * eq2.a

  // Check if the system has a unique solution
  if (determinant == 0) None
  else {
    // Calculate x and y using Cramer's rule
    val x = (eq1.c * eq2.b - eq1.b * eq2.c) / determinant
    val y = (eq1.a * eq2.c - eq1.c * eq2.a) / determinant
    Some((x, y))
  }
}

def createEquations(
    machine: Machine,
    offset: Long = 0
): (Equation, Equation) = {
  (
    Equation(
      machine.buttonA.x,
      machine.buttonB.x,
      (machine.prize.x + offset).toDouble
    ),
    Equation(
      machine.buttonA.y,
      machine.buttonB.y,
      (machine.prize.y + offset).toDouble
    )
  )
}

def part1(machines: List[Machine]): Long = {
  machines
    .map(createEquations(_))
    .flatMap(solve)
    .filter { case (x, y) => x.isWhole && y.isWhole }
    .map { case (a, b) =>
      a * 3 + b * 1
    }
    .map(_.toLong)
    .sum
}

def part2(machines: List[Machine]): Long = {
  machines
    .map(createEquations(_, 10000000000000L))
    .flatMap(solve)
    .filter { case (x, y) => x.isWhole && y.isWhole }
    .map { case (a, b) =>
      a * 3 + b * 1
    }
    .map(_.toLong)
    .sum
}
