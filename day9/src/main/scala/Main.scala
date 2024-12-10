import scala.io.Source
import scala.util.{Failure, Success, Using}

@main def day9(): Unit =
  val lines = Using(Source.fromResource("input.txt")) { source =>
    source.getLines().toList
  }

  lines match {
    case Success(value) =>
      println(part1(value))
      println(part2(value))
    case Failure(exception) => println(exception.getMessage)
  }

def part1(diskMap: List[String]): Long = {
  val initial = diskMap.head.map(_.asDigit)
  val files = initial.zipWithIndex.map((c, i) =>
    (c, if i % 2 == 0 then Some(i / 2) else None)
  )
  val blocks = files.flatMap((v, e) => IndexedSeq.fill(v)(e))

  var left = 0
  var right = blocks.length - 1
  var mutableBlocks = files.flatMap((v, e) => IndexedSeq.fill(v)(e))

  while (left < right) {
    (mutableBlocks(left), mutableBlocks(right)) match
      case (None, None) => right -= 1
      case (None, Some(i)) =>
        mutableBlocks =
          mutableBlocks.updated(left, Some(i)).updated(right, None)
        left += 1
        right -= 1
      case (Some(_), _) => left += 1
  }

  mutableBlocks.flatten.zipWithIndex.map(_.toLong * _).sum
}

def part2(diskMap: List[String]): Long = {
  val initial = diskMap.head.map(_.asDigit).toList

  val blocks = initial.zipWithIndex.foldLeft(Vector.empty[Option[Int]]) {
    case (acc, (length, idx)) =>
      if idx % 2 == 0 then acc ++ Vector.fill(length)(Some(idx / 2))
      else acc ++ Vector.fill(length)(None)
  }

  case class FileInfo(id: Int, start: Int, size: Int)
  val (preFiles, lastId, lastStart) = blocks.zipWithIndex
    .foldLeft(
      Vector.empty[FileInfo],
      Option.empty[Int],
      Option.empty[Int]
    ) { case ((files, currentId, startPos), (block, idx)) =>
      (currentId, startPos) match {
        case (Some(id), Some(start)) if !block.contains(id) =>
          // End of a file
          (
            files :+ FileInfo(id, start, idx - start),
            block.map(identity),
            block.map(_ => idx)
          )
        case (None, None) if block.isDefined =>
          // Start of a file
          (files, block.map(identity), block.map(_ => idx))
        case _ =>
          (files, currentId.orElse(block.map(identity)), startPos)
      }
    }

  // Handle the last file if one exists
  val files = (lastId, lastStart) match {
    case (Some(id), Some(start)) =>
      // Add the final file that extends to the end of the blocks
      preFiles :+ FileInfo(id, start, blocks.length - start)
    case _ => preFiles
  }

  val mutableBlocks = blocks.toArray

  def findEmptySpace(
      blocks: Array[Option[Int]],
      size: Int,
      beforePos: Int
  ): Option[Int] = {
    val noneSequence = Array.fill(size)(None)

    val index = blocks.indexOfSlice(noneSequence)
    if (index >= 0) Some(index) else None
  }

  for {
    file <- files.sortBy(-_.id)
    emptyStart <- findEmptySpace(mutableBlocks, file.size, file.start)
  } {

    // Move the file if we found a valid empty space
    if (emptyStart < file.start) {
      // Clear old position
      for (i <- file.start until file.start + file.size) {
        mutableBlocks(i) = None
      }
      // Set new position
      for (i <- 0 until file.size) {
        mutableBlocks(emptyStart + i) = Some(file.id)
      }
    }
  }

  mutableBlocks.zipWithIndex.collect { case (Some(id), pos) =>
    pos.toLong * id
  }.sum
}
